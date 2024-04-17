#' Parse RMarkdown Year Parameters
#'
#' This is a convenience function to determine the set of years for which the
#' RMarkdown script has to be run. This is useful when the RMarkdown script
#' independently executes a function for each year.
#'
#' @param params The `params` object made available by RMarkdown. It should
#' have the fields `years` (a comma-separated string of years), `year_start`
#' or `year_end`. If `years` is different from `NULL` (`~` in YAML), `years`
#' takes precedence over `year_start` and `year_end`.
#'
#' @return An integer vector of years.
#'
#' @export
parse_year_params <- function(params) {
  stopifnot(all(c("year_start", "year_end") %in% names(params)) || "years" %in% names(params))
  if (is.null(params$years)) {
    year_start <- params$year_start |> as.integer()
    year_end <- params$year_end |> as.integer()
    year_start:year_end
  } else {
    stringr::str_split_1(params$years, ",") |> as.integer()
  }
}
