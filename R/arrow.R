arrow_dataset_files <- function(sources) {
  arrow::open_dataset(sources)$files |>
    purrr::map(\(file) {
      file |>
        stringr::str_split_1("/") |>
        purrr::keep(\(x) stringr::str_detect(x, "=")) |>
        purrr::map(\(kv_string) {
          tibble::as_tibble_col(
            column_name = kv_string |> stringr::str_extract("^([^=]+)=", group = 1),
            x = kv_string |> stringr::str_extract("=([^=]+)$", group = 1)
          )
        }) |>
        purrr::list_cbind()
    }) |>
    purrr::list_rbind()
}



#' Filter Dataset for Latest Release
#'
#' This function determines the latest release of a dataset that has been
#' partitioned by date and applies a filter for the latest release.
#'
#' @param ds A `Dataset` as returned by [arrow:open_dataset].
#' @param release_column The name of a column which can be interpreted as
#' a release date. Optimally, the dataset has been partitioned along this
#' column so that only the latest release data has to be collected in a
#' subsequent step.
#'
#' @returns A `Dataset` with filter for `<release_column> == <latest_release>`.
#'
#' @export
arrow_latest_release <- function(ds, release_column = "release") {
  releases <- ds |>
    dplyr::distinct(.data[[release_column]]) |>
    dplyr::collect() |>
    dplyr::pull(.data[[release_column]]) |>
    lubridate::as_date()

  latest_release <- max(releases)

  ds |>
    dplyr::filter(.data[[release_column]] == .env$latest_release)
}



#' Apply a function to partitioned data sets
#'
#' @param dataset_path_list A named vector with paths to partitioned
#' [arrow::Dataset].
#' @param callback A function with at least the arguments `ds` and `chunk`. `ds`
#' is a named list of data frames. The names are the names of
#' `dataset_path_list`. `chunk` is a 1-based sequence counting up.
#' @param chunk_column_name A character vector of length 1 or a named character
#' vector of `length(dataset_path_list)` with the same names as
#' `dataset_path_list`. Each element is the column name of the partitioned
#' column.
#' @param max_index Maximum number of combinations of partitioned elements to
#' iterate over.
#' @param ... Arguments which are passed to `callback`.
#'
#' @export
arrow_map_chunked <- function(
    dataset_path_list, callback,
    chunk_column_name = "chunk",
    max_index = Inf,
    ...
) {
  stopifnot(rlang::is_named(dataset_path_list))

  chunk_column_names <- if (length(chunk_column_name) == 1) {
    rep(chunk_column_name, times = length(dataset_path_list)) |>
      setNames(names(dataset_path_list))
  } else if (length(chunk_column_name) != length(dataset_path_list)) {
    cli::cli_abort("length(chunk_column_name) has to either be equal to 1 or length(dataset_path_list).")
  } else {
    # Make sure that order matches the order of dataset_path_list
    chunk_column_name[names(dataset_path_list)]
  }

  # Gather set of chunks per dataset
  chunk_list <- purrr::map2(
    dataset_path_list, chunk_column_names,
    \(dataset_path, chunk_column_name) {
      arrow::open_dataset(dataset_path) |>
        dplyr::distinct(.data[[chunk_column_name]]) |>
        dplyr::collect() |>
        dplyr::pull(.data[[chunk_column_name]])
    }
  )

  # Build table that has all permutations of chunks across datasets
  chunk_combs_tbl <- do.call(tidyr::expand_grid, chunk_list)

  # Build a list where each element is a list with one element for each dataset
  # containing the keys "dataset_path" and "chunk"
  chunk_combs_list <- purrr::pmap(chunk_combs_tbl, \(...) {
    l <- list(...)
    purrr::imap(dataset_path_list, \(dataset_path, dataset_path_name) {
      list(
        dataset_path = dataset_path,
        chunk = l[[dataset_path_name]],
        chunk_column_name = chunk_column_names[[dataset_path_name]]
      )
    })
  })

  # Keep less than max_index elements
  max_index <- min(length(chunk_combs_list), max_index)
  chunk_combs_list <- chunk_combs_list[seq_len(max_index)]

  # Read datasets per chunk combination and apply callback
  chunk_combs_list |> purrr::iwalk(
    \(l, i) {
      datasets <- purrr::map(l, \(el) {
        arrow::open_dataset(el$dataset_path) |>
          dplyr::filter(.data[[el$chunk_column_name]] == el$chunk) |>
          dplyr::collect()
      })

      args <- list(
        ds = datasets,
        chunk = i
      )
      args <- c(args, list(...))

      do.call(callback, args)

      if (interactive()) Sys.sleep(0)
    },
    .progress = TRUE
  )
}
