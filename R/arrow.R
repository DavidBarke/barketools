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
