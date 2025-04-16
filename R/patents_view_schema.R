#' Get PatentsView Schema
#'
#' This function converts the data download dictionary from PatentsView on
#' https://patentsview.org/download/data-download-dictionary into a tibble.
#'
#' @export
compute_patents_view_schema <- function(
  data_download_dictionary_url = "https://patentsview.org/download/data-download-dictionary"
) {
  ddd_html <- rvest::read_html(
    httr::GET(
      data_download_dictionary_url,
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
      )
    )
  )
  ddd_tables <- rvest::html_nodes(ddd_html, "tbody") |>
    rvest::html_table() |>
    purrr::keep(\(table) nrow(table) > 0) |>
    purrr::map(\(table) {
      table_name <- table[1,1][[1]]

      if (table_name == "g_persistent_inventor") {
        # 2024-11-08: PatentsView misses the columm name row for this table
        table <- table |>
          dplyr::add_row(
            X1 = "Data Element Name",
            X2 = "Definition",
            X3 = "Example",
            X4 = "Years Present",
            X5 = "Type",
            .after = 1
          )
      }

      table |>
        janitor::row_to_names(2) |>
        janitor::clean_names() |>
        dplyr::mutate(
          table = table_name
        ) |>
        dplyr::filter(data_element_name != "")
    }) |>
    purrr::list_rbind() |>
    dplyr::relocate(
      table,
      column = data_element_name,
      column_type = type
    ) |>
    dplyr::mutate(
      r_column_type = dplyr::case_when(
        stringr::str_detect(column_type, "^(big)?int") ~ "integer",
        column_type == "date" ~ "Date",
        .default = "character"
      )
    )
}



#' PatentsView Schema
#'
#' This dataset is a precomputed version of [patents_view_schema()].
#'
#' @format
#' A data frame with 214 rows and 7 columns:
#' \describe{
#'   \item{table}{Table name, e.g., `"g_applicant_not_disambiguated"`}
#'   \item{column}{Column name, e.g., `"patent_id"`}
#'   \item{column_type}{SQL column type, e.g., `"varchar(36)"`}
#'   \item{definition}{Column definition}
#'   \item{example}{Example value}
#'   \item{years_present}{Years for which data is present}
#'   \item{r_column_type}{`"character"`, `"integer"`, or `"date"`}
#' }
#'
#' @source https://patentsview.org/download/data-download-dictionary
"patents_view_schema"
