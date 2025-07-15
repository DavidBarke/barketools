#' Pad GVKEY
#'
#' Ensure that GVKEY has six digits. Fill missing digits with 0 from the left.
#'
#' @param gvkey A character vector of GVKEYs.
#'
#' @returns A character vector of GVKEYs with exactly six digits.
#'
#' @export
pad_gvkey <- function(gvkey) {
  if (!all(stringr::str_detect(gvkey, "^\\d{4,6}$"), na.rm = TRUE)) {
    cli::cli_abort("'gvkey' has to consist of 4 to 6 digits.")
  }
  stringr::str_pad(gvkey, 6, "left", "0")
}



#' Download Compustat Financials
#'
#' This function downloads financials from the North America and Global
#' Compustat file.
#'
#' @param wrds A database connection to WRDS. See [wrds_connect].
#' @param columns A character vector containing names of columns to download
#' from `comp_global_daily_all.funda` and `comp_global_daily.g_funda`.
#' @param indl A character vector containing industry formats for which
#' financials should be returned. Valid elements are `"INDL"` (industrial) and
#' `"FS"` (financial services).
#' @param consol A character vector containing consolidation levels for which
#' financials should be returned. Valid elements are `"C"` (consolidated),
#' `"D"` (domestic), `"P"` (parent company only / unconsolidated),
#' `"N"` (primary consolidated) and `"R"` (restated consolidated).
#'
#' @export
compustat_get_financials <- function(
  wrds,
  columns,
  indfmt = "INDL",
  consol = "C"
) {
  valid <- indfmt %in% c("INDL", "FS")
  if (!all(valid)) {
    cli::cli_abort(
      paste0("\"", indfmt[!valid][1], "\" is not valid for `indfmt`.")
    )
  }

  valid <- consol %in% c("C", "D", "P", "N", "R")
  if (!all(valid)) {
    cli::cli_abort(
      paste0("\"", consol[!valid][1], "\" is not valid for `consol`.")
    )
  }

  columns <- unique(
    c(
      "datadate",
      "gvkey",
      "datafmt",
      "consol",
      "indfmt",
      columns
    )
  )

  indfmt_filter <- glue::glue(
    "indfmt IN ({indfmt})",
    indfmt = paste0("'", indfmt, "'", collapse = ",")
  )

  consol_filter <- glue::glue(
    "consol IN ({consol})",
    consol = paste0("'", consol, "'", collapse = ",")
  )

  financials <- dbGetQuery(
    wrds,
    "WITH compustat_na AS (
      SELECT DISTINCT
      {columns}
      FROM comp_na_daily_all.funda
    ), compustat_global AS (
      SELECT DISTINCT
      {columns}
      FROM comp_global_daily.g_funda
    ), compustat AS (
      (SELECT *, 'north_america' AS src FROM compustat_na)
      UNION
      (SELECT *, 'global' AS src FROM compustat_global)
    )
    SELECT
      src,
      {columns}
    FROM compustat
    WHERE
      (
        (src = 'north_america' AND compustat.datafmt = 'STD')
        OR (src = 'global' AND compustat.datafmt = 'HIST_STD')
      )
      AND {indfmt_filter}
      AND {consol_filter}
    " |> glue::glue(
      columns = paste0(columns, collapse = ","),
      indfmt_filter = indfmt_filter,
      consol_filter = consol_filter
    )
  ) |>
    as_tibble()

  financials
}

#' Annualize Compustat Financials
#'
#' @param financials Compustat financials as returned by
#' [compustat_get_financials].
#' @param flow_variables Character vector of flow variables. For example,
#' `"capx"` or `"revt"`.
#' @param stock_variables Character vector of stock variables. For example,
#' `"at"` or `"csoh"`.
#'
#' @details
#' Assumptions:
#'
#' * Flow variables are spread uniformly across months, i.e., if a firm files
#' their annual report at the end of march, 3/12 of a flow variable is
#' counted for t and 9/12 is counted for t-1.
#' * Flow variables are reported for the period between previous fiscal year end
#' and current fiscal year end, i.e., if a company changes the month of their
#' fiscal year from June in 1994 to March in 1995 (as was the case for Toyota), the
#' flow only accrues for the nine months between July 1994 and March 1995.
#' TODO: THERE MIGHT BE SOME FIRMS THAT REPORT THE PREVIOUS 12 MONTH FLOW. COULD
#' BE DETECTED BY FILTERING FOR FISCAL YEAR END CHANGES AND LOOKING FOR UNUSUALLY
#' BIG JUMPS IN FLOW VARIABLES (COULD LOOK AT CORRELATION ACROSS ALL FLOW VARIABLES
#'                              FOR HIGHER CERTAINTY)
#' * Stock variables grow linearly between fiscal year end of t-1 and t. Then,
#' stock at end of calendar year can be determined by linear interpolation.
#' * Each month length is 1/12 of year length
#'
#' Need to check what happens if month of fiscal year end changes for a given firm
#'
#' @export
compustat_annualize_financials <- function(
  financials,
  flow_variables,
  stock_variables
) {
  stopifnot("curcd" %in% names(financials))

  financials_now_prev <- financials |>
    arrange(gvkey, src, curcd, datadate) |>
    mutate(
      filing_year = year(datadate),
      filing_month = month(datadate)
    ) |>
    mutate(
      .by = c(gvkey, src, curcd),
      prev_filing_year = lag(filing_year, default = 0),
      prev_filing_month = lag(filing_month, default = 0),
      next_filing_year = lead(filing_year, default = Inf),
      next_filing_month = lead(filing_month, default = Inf)
    ) |>
    mutate(
      n_months_since_last_filing = 12,
      n_months_until_next_filing =
        12 * (next_filing_year != filing_year) +
        (next_filing_month - filing_month),
      n_months_until_calendar_year_end = 12 - filing_month,
      # Flow Variables
      across(
        all_of(flow_variables),
        list(
          share_now = \(x) if_else(
            prev_filing_year == filing_year,
            x,
            (x / n_months_since_last_filing) * filing_month
          ),
          share_prev = \(x) if_else(
            prev_filing_year == filing_year,
            0,
            (x / n_months_since_last_filing) * (12 - prev_filing_month)
          )
        )
      ),
      # Stock Variables
      across(
        all_of(stock_variables),
        list(
          stock = \(x) {
            next_x <- lead(x)
            if_else(
              next_filing_year == filing_year,
              NA,
              x + (next_x - x) *
                n_months_until_calendar_year_end / n_months_until_next_filing
            )
          }
        )
      )
    )

  financials_flow_now <- financials_now_prev |>
    select(gvkey, curcd, src, year = filing_year, ends_with("_share_now")) |>
    rename_with(\(x) x |> str_remove("_share_now"), ends_with("_share_now"))

  financials_flow_prev <- financials_now_prev |>
    select(gvkey, curcd, src, year = prev_filing_year, ends_with("_share_prev")) |>
    rename_with(\(x) x |> str_remove("_share_prev"), ends_with("_share_prev")) |>
    filter(!if_all(all_of(flow_variables), \(x) x == 0 | is.na(x)))

  financials_stock <- financials_now_prev |>
    select(gvkey, curcd, src, year = filing_year, ends_with("_stock")) |>
    rename_with(\(x) x |> str_remove("_stock"), ends_with("_stock")) |>
    filter(!if_any(all_of(stock_variables), is.na))

  # Compute min_year and max_year per GVKEY and use it to remove years for which
  # we'd only have observations in one of two years
  financials_min_max_year <- financials |>
    mutate(
      month = month(datadate),
      year = year(datadate)
    ) |>
    arrange(gvkey, year) |>
    summarize(
      .by = c(gvkey, src),
      min_year = min(year),
      max_year = max(year),
      last_month = last(month),
      # max_year does not need to be respected if filing month was December
      # because then all month of the max year are covered
      max_year_complete = if_else(last_month == 12, max_year, max_year - 1)
    )

  financials_annualized <- bind_rows(
    financials_flow_now,
    financials_flow_prev
  ) |>
    summarize(
      .by = c(gvkey, curcd, src, year),
      across(
        all_of(flow_variables),
        \(x) if (any(!is.na(x))) sum(x, na.rm = TRUE) else NA
      )
    ) |>
    left_join(financials_min_max_year, join_by(gvkey, src)) |>
    filter(year >= min_year, year <= max_year_complete) |>
    left_join(financials_stock, join_by(gvkey, curcd, src, year))

  financials_annualized_2 <- financials_annualized |>
    select(
      gvkey, curcd, src, year,
      all_of(flow_variables),
      all_of(stock_variables)
    )

  # Sanity check: one row per gvkey, src, curcd, year
  n_dupl <- financials_annualized_2 |>
    filter(n() > 1, .by = c(gvkey, src, curcd, year)) |>
    nrow()

  if (n_dupl > 0) {
    cli::cli_alert_warning("Data is not identified on gvkey-src-curcd-year level!")
  }

  financials_annualized_2
}
