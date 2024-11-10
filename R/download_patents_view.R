#' Read a table from PatentsView
#'
#' Downloads zipped table from patentsview.org, unzips it, and returns the path
#' to the unzipped file.
#'
#' @param table Table name, e.g., `g_location_disambiguated`.
#'
#' @returns Path to the unzipped tsv file.
#'
#' @export
download_patents_view <- function(
  table = c(
    "g_applicant_not_disambiguated",
    "g_application",
    "g_assignee_disambiguated",
    "g_assignee_not_disambiguated",
    "g_attorney_disambiguated",
    "g_attorney_not_disambiguated",
    "g_botanic",
    "g_brf_sum_text",
    "g_claim",
    "g_cpc_current",
    "g_cpc_title",
    "g_detail_desc_text",
    "g_draw_desc_text",
    "g_examiner_not_disambiguated",
    "g_figures",
    "g_foreign_citation",
    "g_foreign_priority",
    "g_gov_interest",
    "g_gov_interest_contracts",
    "g_gov_interest_org",
    "g_inventor_disambiguated",
    "g_inventor_not_disambiguated",
    "g_ipc_at_issue",
    "g_location_disambiguated",
    "g_location_not_disambiguated",
    "g_other_reference",
    "g_patent",
    "g_patent_abstract",
    "g_pct_data",
    "g_persistent_assignee",
    "g_persistent_inventor",
    "g_rel_app_text",
    "g_us_application_citation",
    "g_us_patent_citation",
    "g_us_rel_doc",
    "g_us_term_of_grant",
    "g_uspc_at_issue",
    "g_wipo_technology"
  )
) {
  table <- match.arg(table)

  folder <- if (stringr::str_detect(table, "^pg")) {
    "pregrant_publications"
  } else if (stringr::str_detect(table, "^g_brf")) {
    "brief-summary-text"
  } else if (stringr::str_detect(table, "^g_claims")) {
    "claims"
  } else if (stringr::str_detect(table, "^g_detail_desc_text")) {
    "detail-description-text"
  } else if (stringr::str_detect(table, "^g_draw_desc_text")) {
    "draw-description-text"
  } else if (stringr::str_detect(table, "^g")) {
    "download"
  } else {
    cli::cli_abort("Table '{table}' does not fit the PatentsView schema.")
  }

  pv_path <- file.path(
    "https://s3.amazonaws.com/data.patentsview.org",
    folder,
    paste0(table, ".tsv.zip")
  )

  tf <- tempfile(fileext = ".tsv.zip")
  download.file(pv_path, destfile = tf)

  td <- tempdir()
  unzipped_file_path <- unzip(tf, exdir = td)
  file.remove(tf)

  unzipped_file_path
}



#' Write a table from PatentsView to S3
#'
#' Downloads zipped table from patentsview.org, unzips it, reads it into R
#' using [readr::read_tsv], and writes it to `s3_out`.
#'
#' @inheritParams download_patents_view
#' @param s3_out S3 path, e.g., `s3://<bucket>/path`
#'
#' @export
download_patents_view_to_s3 <- function(
    table, s3_out
) {
  tsv_path <- download_patents_view(table = table)

  df <- data.table::fread(
    tsv_path,
    header = TRUE,
    sep = "\t",
    colClasses = patents_view_col_classes(table) |> unname(),
    col.names = patents_view_col_classes(table) |> names()
  )
  file.remove(tsv_path)

  arrow::write_dataset(
    df,
    s3_out
  )
}



patents_view_col_classes <- function(table) {
  df <- patents_view_schema |>
    dplyr::filter(table == .env$table)
  column_names <- df$column
  r_column_type <- df$r_column_type
  names(r_column_type) <- column_names
  r_column_type
}



#' Write a table from PatentsView to S3
#'
#' Downloads zipped table from patentsview.org, unzips it, reads it into R
#' using [readr::read_tsv_chunked], and writes each chunk to `s3_out`.
#'
#' @inheritParams download_patents_view_to_s3
#' @param chunk_size The number of rows to include in each chunk.
#' @param chunk_column_name The column name that will be used to partition the
#' data on S3.
#'
#' @export
download_patents_view_to_s3_chunked <- function(
    table, s3_out, chunk_size = 1e5, chunk_column_name = "chunk"
) {
  tsv_path <- download_patents_view(table = table)

  fread_chunked(
    tsv_path,
    chunk_size = chunk_size,
    callback = \(x, chunk) {
      x[[chunk_column_name]] <- chunk

      arrow::write_dataset(
        x,
        s3_out,
        partitioning = chunk_column_name
      )
    },
    sep = "\t",
    colClasses = patents_view_col_classes(table) |> unname(),
    col.names = patents_view_col_classes(table) |> names()
  )
}



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
#' @param header Argument passed to [data.table::fread]. Currently, only
#' `FALSE` is supported, so that it is suggested to supply `col.names` as well.
#' @param verbose If `TRUE`, print progress information.
#' @param ... Arguments passed to [data.table::fread].
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

  stopifnot(!header)

  chunk <- 0
  done <- FALSE
  while (TRUE) {
    tryCatch(
      {
        if (verbose) cli::cli_alert_info("Reading chunk {chunk}")
        x <- data.table::fread(
          file,
          skip = chunk * chunk_size + (!header),
          nrows = chunk_size,
          header = header,
          ...
        )

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
