#' Repair PatentsView Filing Dates
#'
#' This function repairs filing dates of patents in PatentsView. Consider for
#' example, patent [US4469216](https://patents.google.com/patent/US4469216A)
#' which has a recorded filing date of `Apr. 5, 8198`. The corrected filing
#' date is `Apr. 5, 1982`. All corrected filing dates were manually confirmed.
#'
#' @param filing_dates A vector of filing dates.
#'
#' @export
repair_pv_filing_dates <- function(filing_dates) {
  filing_dates |>
    stringr::str_replace("^(91|10|29|79)", "19") |>
    stringr::str_replace("^(1877)", "1977") |>
    stringr::str_replace("^(1878)", "1978") |>
    stringr::str_replace("^(1868)", "1979") |>
    stringr::str_replace("^(8198|1298|1682|1872)", "1982") |>
    stringr::str_replace("^(1873)", "1983") |>
    stringr::str_replace("^975", "1975") |>
    lubridate::as_date()
}
