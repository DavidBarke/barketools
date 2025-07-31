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



#' Winsorize
#'
#' Winsorize a numerical vector.
#'
#' @inheritParams winsorize_bounds
#'
#' @export
winsorize <- function(x, probs) {
  bounds <- winsorize_bounds(x, probs = probs)
  dplyr::if_else(
    x < bounds[1],
    bounds[1],
    dplyr::if_else(
      x > bounds[2],
      bounds[2],
      x
    )
  )
}



#' Winsorize Bounds
#'
#' Get winsorize bounds for a numerical vector. This function is effectively
#' a wrapper around [stats:quantile].
#'
#' @param x Numerical vector.
#' @param probs A numerical vector of length 2 specifying the quantiles at which
#' to winsorize. Use `0` or `1` if you do not want to winsorize at the lower end
#' or the upper end, respectively.
#'
#' @returns A numerical vector of length 2. The first element is the lower bound
#' and the second element is the upper bound.
#'
#' @export
winsorize_bounds <- function(x, probs) {
  stopifnot(length(probs) == 2)
  quantile(x, probs = probs, type = 1, na.rm = TRUE) |>
    sort() |>
    unname()
}
