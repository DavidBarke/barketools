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

list_s3_files <- function(prefix) {
  load_python_script("./pygcpd/s3/s3_utils.py")
  python_env <- setup_python_env()
  python_env$list_files(prefix)
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
  out <- shell(
    glue::glue(
      "aws s3api head-object",
      "--bucket {bucket}",
      "--key {key}",
      bucket = bucket,
      key = s3_key,
      .sep = " "
    ),
    wait = FALSE,
    intern = TRUE
  ) |> suppressWarnings()

  if (verbose) cat(out)

  (attr(out, "status") %||% 0) != 254
}



#' Apply `s3_key_exists` recursively to S3 keys in a list
#'
#' This function raises an error as soon as any S3 key does not exist.
#'
#' @param l List.
#' @param is_s3_key Function that determines which elements should be treated as S3
#' keys
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



write_json_to_s3 <- function(x, s3_key, bucket = "gcpd") {
  tf <- tempfile(fileext = ".json")
  jsonlite::write_json(x, tf, auto_unbox = TRUE)
  s3_put_object(
    file = tf,
    s3_key = s3_key,
    bucket = bucket
  )
}



s3_put_object <- function(file, s3_key, bucket = "gcpd") {
  shell(
    glue::glue(
      "aws s3api put-object",
      "--bucket {bucket}",
      "--key {key}",
      "--body {body}",
      bucket = bucket,
      key = s3_key,
      body = file,
      .sep = " "
    )
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
