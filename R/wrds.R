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



#' WRDS Schemata
#'
#' Get all available WRDS schemata.
#'
#' @param conn Database connection to WRDS created with [wrds_connect].
#'
#' @returns A tibble with the following column:
#' * `schema_name`: Schema name. Use [wrds_schema_tables] to query all tables
#' under a schema.
#'
#' @export
wrds_schemata <- function(conn) {
  DBI::dbGetQuery(
    conn,
    "SELECT schema_name
    FROM information_schema.schemata
    ORDER BY schema_name"
  ) |>
    tibble::as_tibble()
}



#' WRDS Tables
#'
#' Get all available tables under a schema.
#'
#' @inheritParams wrds_schemata
#' @param schema Name of WRDS schema, e.g., `"crsp"`.
#'
#' @returns A tibble with the following columns:
#' * `table_schema`: `schema`.
#' * `table_name`: Table name. Use [wrds_table_columns] to query column names
#' of a table.
#' * `table_type`:  Table type. One of `"BASE TABLE"`, `"VIEW"` or
#' `"FOREIGN TABLE"`.
#'
#' @export
wrds_schema_tables <- function(conn, schema) {
  DBI::dbGetQuery(
    conn,
    "SELECT table_schema, table_name, table_type
    FROM information_schema.tables
    WHERE table_schema = '{schema}'
    ORDER BY table_name" |>
      glue::glue(schema = schema)
  ) |>
    tibble::as_tibble()
}



#' WRDS Table Columns
#'
#' Get all columns of a WRDS table.
#'
#' @inheritParams wrds_schema_tables
#' @param table Table name within the given schema.
#'
#' @returns A tibble with the following columns:
#' * `table_schema`: `schema`.
#' * `table_name`: `table`.
#' * `column_name`: Column name.
#' * `column_default`: Default value.
#' * `is_nullable`: `"YES"` or `"NO"`.
#' * `data_type`: [Data Type](https://www.postgresql.org/docs/current/datatype.html).
#'
#' @export
wrds_table_columns <- function(conn, schema, table) {
  DBI::dbGetQuery(
    conn,
    "SELECT table_schema, table_name, column_name, column_default, is_nullable,
    data_type
    FROM information_schema.columns
    WHERE table_schema = '{schema}' AND table_name = '{table}'
    ORDER BY column_name" |>
      glue::glue(
        schema = schema,
        table = table
      )
  ) |>
    tibble::as_tibble()
}



#' WRDS Table Count
#'
#' Get number of records in a WRDS table
#'
#' @inheritParams wrds_table_columns
#'
#' @returns An integer.
#'
#' @export
wrds_table_count <- function(conn, schema, table) {
  DBI::dbGetQuery(
    conn,
    "SELECT COUNT(*) AS n
    FROM {schema}.{table}" |>
      glue::glue(
        schema = schema,
        table = table
      )
  ) |>
    tibble::as_tibble() |>
    dplyr::pull(n)
}

