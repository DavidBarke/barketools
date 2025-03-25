#' PatentsView Assignee Types
#'
#' @returns A named character vector. Names are numerical values (1-9) of
#' and values are textual descriptions of assignee types.
#'
#' @export
patents_view_assignee_type_map <- function() {
  assignee_types <- c(
    "Unassigned",
    "US Company or Corporation",
    "Foreign Company or Corporation",
    "US Individual",
    "Foreign Individual",
    "US Federal Government",
    "Foreign Government",
    "US County Government",
    "US State Government"
  )
  names(assignee_types) <- 1:9
  assignee_types
}
