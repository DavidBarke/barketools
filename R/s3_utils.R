setup_python_env <- function() {
  if (!exists("python_env", where = globalenv())) {
    assign("python_env", new.env(), envir = globalenv())
    dotenv::load_dot_env(env_file())
    reticulate::use_condaenv(Sys.getenv("CONDAENV"))
  }

  return(get("python_env", envir = globalenv()))
}

load_python_script <- function(path) {
  python_env <- setup_python_env()
  reticulate::source_python(
    path, envir = python_env
  )
}



#' @export
requires_s3 <- function(path, suggestion = NULL) {
  tryCatch({
    open_dataset(path) |>
      invisible()
  }, error = function(e) {
    cli::cli_alert_info(e)
    if (!is.null(suggestion)) {
      cli::cli_alert_warning("Here is my suggestion: {suggestion}")
    }
  })
}



#' Check whether S3 key exists
#'
#' @param s3_key S3 key.
#'
#' @export
s3_key_exists <- function(s3_key, bucket = "gcpd", verbose = FALSE) {
  s3 <- paws::s3()

  tryCatch(
    {
      s3$head_object(
        Bucket = bucket,
        Key = s3_key
      )
      return(TRUE)
    },
    error = function(e) {
      if (verbose) cli::cli_alert_warning(e)
      return(FALSE)
    }
  )
}



#' Apply `s3_key_exists` recursively to S3 keys in a list
#'
#' This function raises an error as soon as any S3 key does not exist.
#'
#' @param l List.
#' @param predicate A function taking two arguments `l` (an element of a list)
#' and `name` (currently defunct). If this function returns `TRUE`, `transform`
#' is applied on this element of the list.
#' @param transform A function taking one argument `l` (an element of a list).
#' This function should return a character representing a potential S3 key. This
#' character is then passed to [s3_key_exists].
#' @param verbose If `TRUE`, print more output.
#'
#' @export
s3_key_exists_list <- function(
    l,
    predicate = \(l, name) is.character(l),
    transform = identity,
    verbose = FALSE
) {
  apply_s3_key_exists <- function(l, name) {
    if (is.list(l)) {
      purrr::iwalk(l, apply_s3_key_exists)
    } else if (predicate(l, name)) {
      s3_key <- transform(l)
      if (verbose) cli::cli_alert_info("Checking whether S3 key exists: {s3_key}")
      if (!s3_key_exists(s3_key)) {
        cli::cli_abort("S3 key does not exist: {l}")
      }
    }
  }

  apply_s3_key_exists(l, NULL)

  invisible(l)
}



#' Write JSON to S3
#'
#' @param x List.
#' @param s3_key S3 key.
#' @param bucket S3 bucket.
#'
#' @export
write_json_to_s3 <- function(x, s3_key, bucket = "gcpd") {
  tf <- tempfile(fileext = ".json")
  jsonlite::write_json(x, tf, auto_unbox = TRUE)
  s3 <- paws::s3()
  s3$put_object(
    Body = tf,
    Key = s3_key,
    Bucket = bucket
  )
}



s3_merge <- function(s3_keys_in, s3_key_out, bucket = "gcpd") {
  purrr::walk(s3_keys_in, \(s3_key) {
    s3_key_exists(s3_key, bucket = bucket)
  })

  out <- purrr::map(s3_keys_in, \(s3_key) {
    arrow::open_dataset(s3_key) |>
      dplyr::collect()
  }) |>
    purrr::list_rbind()

  arrow::write_dataset(
    out,
    s3_key_out
  )
}



#' List Files on S3
#'
#' @param prefix Common prefix to all files.
#' @param bucket S3 bucket.
#' @param include_folder If `TRUE`, also include S3 keys referring to folders.
#' @param max Maximum number of results to return.
#' @param value See `Value`.
#' @param delimiter Character string used to group keys. Read the AWS docs for
#' more details.
#'
#' @returns If `value == "vector"`, a character vector of S3 keys. If
#' `value == "df"` a data frame of S3 keys.
#'
#' @export
s3_list_files <- function(
    prefix = NULL, bucket = "gcpd", include_folder = FALSE, max = 1e3,
    delimiter = NULL
) {
  s3 <- paws::s3()
  continuation_token <- NULL
  n <- ceiling(max / 1e3)
  keys <- purrr::map(seq_len(n), \(i) {
    if (i > 1 && length(continuation_token) == 0) {
      character()
    } else {
      x <- s3$list_objects_v2(
        Bucket = bucket,
        Prefix = prefix,
        Delimiter = delimiter,
        ContinuationToken = continuation_token
      )

      continuation_token <<- x$NextContinuationToken

      x$Contents |> purrr::map_chr("Key")
    }
  }, .progress = TRUE) |>
    purrr::list_c()

  if (!include_folder) {
    keys <- keys[!stringr::str_detect(keys, "/$")]
  }

  keys
}



#' List S3 Subfolders
#'
#' Subfolders are exactly one level further down than the given prefix.
#'
#' @inheritParams s3_list_files
#'
#' @export
s3_list_subfolders <- function(
    prefix = NULL, bucket = "gcpd", max = 1e3
) {
  s3 <- paws::s3()
  continuation_token <- NULL
  n <- ceiling(max / 1e3)
  purrr::map(seq_len(n), \(i) {
    if (i > 1 && length(continuation_token) == 0) {
      character()
    } else {
      x <- s3$list_objects_v2(
        Bucket = bucket,
        Prefix = prefix,
        Delimiter = "/",
        ContinuationToken = continuation_token
      )

      continuation_token <<- x$NextContinuationToken

      x$CommonPrefixes |> purrr::map_chr("Prefix")
    }
  }, .progress = TRUE) |>
    purrr::list_c()
}



#' Read JSON file from S3
#'
#' @param bucket S3 bucket.
#' @param key S3 key of JSON file.
#'
#' @returns A list representing the contents of the JSON file.
#'
#' @export
s3_read_json <- function(bucket, key) {
  s3 <- paws::s3()
  s3$get_object(
    Bucket = bucket,
    Key = key
  ) |>
    getElement("Body") |>
    rawToChar() |>
    jsonlite::fromJSON(simplifyVector = FALSE)
}



#' Read XLSX file from S3
#'
#' @param bucket S3 bucket.
#' @param key S3 key of XLSX file.
#'
#' @returns A tibble.
#'
#' @export
s3_read_xlsx <- function(bucket, key) {
  s3 <- paws::s3()

  tf <- tempfile()
  s3$download_file(
    Bucket = bucket,
    Key = key,
    Filename = tf
  )

  readxl::read_xlsx(tf)
}



#' Read XLS file from S3
#'
#' @param bucket S3 bucket.
#' @param key S3 key of XLS file.
#'
#' @returns A tibble.
#'
#' @export
s3_read_xls <- function(bucket, key) {
  s3 <- paws::s3()

  tf <- tempfile()
  s3$download_file(
    Bucket = bucket,
    Key = key,
    Filename = tf
  )

  readxl::read_xls(tf)
}

