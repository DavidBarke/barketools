#' PatentsView Assignee Types
#'
#' @returns A named character vector. Names are numerical values (1-9) of
#' and values are textual descriptions of assignee types.
#'
#' @export
patents_view_assignee_type_map <- function() {
  tbl <- patents_view_assignee_type_tbl()
  assignee_types <- tbl$assignee_type_str
  names(assignee_types) <- tbl$assignee_type
  assignee_types
}


#' PatentsView Assignee Types
#'
#' @returns A tibble with the following columns:
#' * `assignee_type` (numeric). PatentsView assignee type.
#' * `assignee_type_2` (character). Assignee category, e.g., `"Firm"` or
#' `"Government"`.
#' * `assignee_type_str` (character). Textual description of assignee type.
#' * `assignee_region` (character). `"US"` or `"Foreign"`.
#' * `partial_interest` (logical). `TRUE` if partial interest.
#'
#' @export
patents_view_assignee_type_tbl <- function() {
  tbl <- tibble::tribble(
    ~assignee_type, ~assignee_type_2, ~assignee_type_str, ~assignee_region,
    0, "0", "0", NA,
    1, "Unassigned", "Unassigned", NA,
    2, "Firm", "US Company or Corporation", "US",
    3, "Firm", "Foreign Company or Corporation", "Foreign",
    4, "Individual", "US Individual", "US",
    5, "Individual", "Foreign Individual", "Foreign",
    6, "Government", "US Federal Government", "US",
    7, "Government", "Foreign Government", "Foreign",
    8, "Government", "US County Government", "US",
    9, "Government", "US State Government", "US",
    12, "Firm", "US Company or Corporation", "US",
    13, "Firm", "Foreign Company or Corporation", "Foreign",
    14, "Individual", "US Individual", "US",
    15, "Individual", "Foreign Individual", "Foreign",
    16, "Government", "US Federal Government", "US",
    17, "Government", "Foreign Government", "Foreign",
    18, "Government", "US County Government", "US",
    19, "Government", "US State Government", "US"
  )
  tbl$partial_interest <- tbl$assignee_type %in% 12:19
  tbl
}



#' PatentsView: Last Updated
#'
#' This function returns the "Last Updated" date for every PatentsView table.
#'
#' @export
patents_view_last_updated <- function() {
  .patents_view_last_updated_tbl() |>
    tidyr::unnest(last_updated_tbl) |>
    dplyr::select(type, table, indexed_by_year, last_updated)
}



.patents_view_last_updated_tbl <- function() {
  purrr::pmap(patents_view_urls(), \(type, url, indexed_by_year) {
    resp <- httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")
    )

    html <- rvest::read_html(resp)

    if (indexed_by_year) {
      table <- html |>
        rvest::html_elements(".data-grid p") |>
        rvest::html_text()
      table <- paste(type, table, sep = "_")

      last_updated <- html |>
        rvest::html_elements(".data-grid div.file-date") |>
        rvest::html_text() |>
        stringr::str_extract("^Updated: (.*)$", group = 1) |>
        lubridate::mdy() |>
        suppressWarnings()
    } else if (type == "beta") {
      rows <- html |>
        rvest::html_elements(".beta-download-table tr")

      table <- rows |>
        purrr::map(\(row) {
          row |>
            html_elements("td")
        }) |>
        purrr::keep(\(x) length(x) > 0) |>
        purrr::map(1) |>
        structure(class = c("xml_nodeset", "xml_node")) |>
        rvest::html_element("a") |>
        rvest::html_text()

      last_updated <- rows |>
        purrr::map(\(row) {
          row |>
            html_elements("td")
        }) |>
        purrr::keep(\(x) length(x) > 0) |>
        purrr::map(5) |>
        purrr::map_chr(rvest::html_text) |>
        lubridate::mdy()
    } else {
      table <- html |>
        rvest::html_elements(".file-title a") |>
        rvest::html_text()

      last_updated <- html |>
        rvest::html_elements("time") |>
        rvest::html_attr("datetime") |>
        lubridate::ymd_hms() |>
        lubridate::as_date()
    }

    last_updated_tbl <- tibble::tibble(
      table = table,
      last_updated = last_updated
    )

    tibble::tibble(
      type = type,
      html = list(html),
      last_updated_tbl = list(last_updated_tbl),
      indexed_by_year = indexed_by_year
    )
  }) |>
    purrr::list_rbind()
}



#' PatentsView URLs
#'
#' This function returns the URLs associated with different sets of PatentsView
#' tables.
#'
#' @export
patents_view_urls <- function() {
  tibble::tribble(
    ~type, ~url, ~indexed_by_year,
    "granted", "https://patentsview.org/download/data-download-tables", FALSE,
    "g_brf_sum_text", "https://patentsview.org/download/brf_sum_text", TRUE,
    "g_claims", "https://patentsview.org/download/claims", TRUE,
    "g_detail_desc_text", "https://patentsview.org/download/detail_desc_text", TRUE,
    "g_draw_desc_text", "https://patentsview.org/download/draw_desc_text", TRUE,
    "pregrant", "https://patentsview.org/download/pg-download-tables", FALSE,
    "pg_brf_sum_text", "https://patentsview.org/download/pg_brf_sum_text", TRUE,
    "pg_claims", "https://patentsview.org/download/pg_claims", TRUE,
    "pg_detail_desc_text", "https://patentsview.org/download/pg_detail_desc_text", TRUE,
    "pg_draw_desc_text", "https://patentsview.org/download/pg_draw_desc_text", TRUE,
    "beta", "https://patentsview.org/download/data-download-tables-beta", FALSE
  )
}
