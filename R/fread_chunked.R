#' Chunked Fread
#'
#' Chunked version of [data.table::fread].
#'
#' @param file File name in working directory, path to file (passed through
#' [base::path.expand] for convenience), or a URL starting
#' http://, file://, etc. Compressed files with extension ‘.gz’ and ‘.bz2’ are
#' supported if the R.utils package is installed.
#' @param chunk_size Number of rows that is read per chunk.
#' @param callback A function taking two arguments `x` (the data of the current
#' chunk) and `chunk` (the zero-based number of the current chunk). Use this
#' function to process the chunked data, e.g., by transforming it and writing
#' it to a database.
#' @param header If `TRUE`, column names are taken from the first row and
#' supplied as `col.names` argument to the `fread` call for each chunk.
#' @param verbose If `TRUE`, print progress information.
#' @param ... Arguments passed to [data.table::fread].
#'
#' @returns An integer. The zero-based number of chunks.
#'
#' @export
fread_chunked <- function(
    file, chunk_size, callback, header = FALSE, verbose = TRUE, ...
) {
  if (formals(callback) |> length() != 2) {
    cli::cli_abort(
      "'callback' has to be a function that takes two arguments:
      'x' (the chunk that has been read) and 'chunk' (the zero-based index
      of the chunk that has been read)"
    )
  }

  if (header & "col.names" %in% names(list(...))) {
    cli::cli_abort(
      "`col.names` must not be set when `header` is `TRUE`."
    )
  }

  if (!header & "colClasses" %in% names(list(...))) {
    colClasses <- list(...)$colClasses
    n_cols <- data.table::fread(file, nrows = 0) |>
      length()
    if (length(colClasses) != n_cols) {
      cli::cli_abort(
        "colClasses must be a vector whose length is equal to the number of
        columns if `header` is `FALSE`."
      )
    }
  }

  col.names <- if (header) {
    data.table::fread(file, nrows = 0) |>
      names()
  }

  chunk <- 0
  done <- FALSE
  while (TRUE) {
    tryCatch(
      {
        if (verbose) cli::cli_alert_info("Reading chunk {chunk}")
        x <- if (is.null(col.names)) {
          data.table::fread(
            file,
            skip = chunk * chunk_size + (!header),
            nrows = chunk_size,
            header = FALSE,
            ...
          )
        } else {
          data.table::fread(
            file,
            skip = chunk * chunk_size + (!header),
            nrows = chunk_size,
            header = FALSE,
            col.names = col.names,
            ...
          )
        }

        callback(x, chunk)

        chunk <- chunk + 1
      },
      error = function(e) {
        if (stringr::str_detect(e$message, "but the input only has \\d+ lines")) {
          if (verbose) cli::cli_alert_info("No more lines to read.")
          done <<- TRUE
        } else {
          stop(e)
        }
      },
      warning = function(w) {
        stop(w)
      }
    )

    if (done) break
  }

  chunk
}
