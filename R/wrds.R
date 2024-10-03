#' Connect to WRDS
#'
#' Make sure to setup your [local RStudio](https://wrds-www.wharton.upenn.edu/pages/support/programming-wrds/programming-r/r-from-the-web/)
#' or [RStudioServer](https://wrds-www.wharton.upenn.edu/pages/support/programming-wrds/programming-r/r-from-the-web/)
#' as described on the linked websites.
#'
#' @param user Character. WRDS user name.
#'
#' @returns A `DBI` connection to WRDS.
#'
#' @export
wrds_connect <- function(user) {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host='wrds-pgdata.wharton.upenn.edu',
    port=9737,
    dbname='wrds',
    sslmode='require',
    user=user
  )
}



#' WRDS Libraries
#'
#' Get a list of all available WRDS libraries.
#'
#' @param conn Connection to WRDS from [wrds_connect()].
#'
#' @returns A tibble with column `table_schema`.
#'
#' @export
wrds_libraries <- function(conn) {
  DBI::dbGetQuery(
    conn,
    "SELECT DISTINCT table_schema
    FROM information_schema.tables
    WHERE table_type ='VIEW' OR table_type ='FOREIGN TABLE'
    ORDER BY table_schema"
  ) |>
    tibble::as_tibble()
}



#' WRDS Datasets
#'
#' Get all available datasets within a given WRDS library.
#'
#' @inheritParams wrds_libraries
#' @param library Name of WRDS library, e.g., `"crsp"`.
#'
#' @returns A tibble with column `table_name`.
#'
#' @export
wrds_datasets <- function(conn, library) {
  DBI::dbGetQuery(
    conn,
    "SELECT DISTINCT table_name
    FROM information_schema.columns
    WHERE table_schema = '{library}'
    ORDER BY table_name" |>
      glue::glue(library = library)
  ) |>
    tibble::as_tibble()
}



#' WRDS Dataset Columns
#'
#' Get all columns of a WRDS dataset.
#'
#' @export
wrds_dataset_columns <- function(conn, library, dataset) {
  DBI::dbGetQuery(
    conn,
    "SELECT column_name
    FROM information_schema.columns
    WHERE table_schema = '{library}' AND table_name = '{dataset}'
    ORDER BY column_name" |>
      glue::glue(
        library = library,
        dataset = dataset
      )
  ) |>
    tibble::as_tibble()
}




wrds_dataset_count <- function(conn, library, dataset) {
  DBI::dbGetQuery(
    conn,
    "SELECT COUNT(*)
    FROM {library}.{dataset}" |>
      glue::glue(
        library = library,
        dataset = dataset
      )
  ) |>
    tibble::as_tibble()
}

