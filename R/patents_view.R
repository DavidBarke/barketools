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
#' * `assignee_type_str` (character). Textual description of assignee type.
#' * `partial_interest` (logical). `TRUE` if partial interest.
#'
#' @export
patents_view_assignee_type_tbl <- function() {
  tbl <- tibble::tribble(
    ~assignee_type, ~assignee_type_str,
    0, "0",
    1, "Unassigned",
    2, "US Company or Corporation",
    3, "Foreign Company or Corporation",
    4, "US Individual",
    5, "Foreign Individual",
    6, "US Federal Government",
    7, "Foreign Government",
    8, "US County Government",
    9, "US State Government",
    12, "US Company or Corporation",
    13, "Foreign Company or Corporation",
    14, "US Individual",
    15, "Foreign Individual",
    16, "US Federal Government",
    17, "Foreign Government",
    18, "US County Government",
    19, "US State Government"
  )
  tbl$partial_interest <- tbl$assignee_type %in% 12:19
  tbl
}
