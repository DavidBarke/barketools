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
