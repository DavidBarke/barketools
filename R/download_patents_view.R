#' Read a table from PatentsView
#'
#' Downloads zipped table from patentsview.org, unzips it, and reads it
#' into R using [readr::read_tsv].
#'
#' @param table Table name, e.g., `g_location_disambiguated`.
#'
#' @export
download_patents_view <- function(table) {
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
  unzipped_file <- unzip(tf, exdir = td)

  readr::read_tsv(unzipped_file, show_col_types = FALSE)
}



#' Write a table from PatentsView to S3
#'
#' Downloads zipped table from patentsview.org, unzips it, reads it into R
#' using [readr::read_tsv], and writes it to `s3_out`.
#'
#' @inheritParams download_patents_view
#' @param s3_out S3 path.
#'
#' @export
download_patents_view_to_s3 <- function(table, s3_out) {
  df <- download_patents_view(
    table = table
  )

  arrow::write_dataset(df, s3_out)
}
