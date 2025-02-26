#' Empirical CDF
#'
#' Create a tibble with two columns: `q`: Quantile, `F_q`: Empirical CDF at `q`.
#'
#' @param x Numeric vector.
#' @param n Number of quantiles for which to compute `F_q` between `fmin(x)` and
#' `fmax(x)`.
#' @param fmin Function that returns the minimum quantile.
#' @param fmax Function that returns the maximum quantile.
#'
#' @export
ecdf_tbl <- function(x, n = 100, fmin = min, fmax = max) {
  cdf <- ecdf(x)
  tibble::tibble(
    q = seq(fmin(x), fmax(x), length.out = n)
  ) |>
    dplyr::mutate(F_q = cdf(q))
}
