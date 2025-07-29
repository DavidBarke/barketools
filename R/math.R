#' @export
max_or_na <- function(x) {
  if (all(is.na(x))) return(NA)
  max(x, na.rm = TRUE)
}



#' @export
min_or_na <- function(x) {
  if (all(is.na(x))) return(NA)
  min(x, na.rm = TRUE)
}



#' @export
psum <- function(..., na.rm = TRUE) {
  purrr::pmap(list(...), \(...) {
    sum(..., na.rm = na.rm)
  })
}



#' @export
pprod <- function(..., na.rm = TRUE) {
  purrr::pmap(list(...), \(...) {
    prod(..., na.rm = na.rm)
  })
}



#' @export
pall <- function(..., na.rm = TRUE) {
  purrr::pmap(list(...), \(...) {
    all(..., na.rm = na.rm)
  })
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
