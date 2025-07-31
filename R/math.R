#' Return NA if all NA
#'
#' Variants of standard functions that return [base:NA] if all elements of
#' `x` are `NA`.
#'
#' @param x A vector.
#' @name or_na
NULL



#' @rdname or_na
#' @export
max_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}



#' @rdname or_na
#' @export
min_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}



#' @rdname or_na
#' @export
mean_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}



#' @rdname or_na
#' @export
sum_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}



#' Parallel Computations
#'
#' Variants of standard functions that are what [base:pmax] is to
#' [base:max].
#'
#' @param ... Vectors of the same length.
#' @param na.rm A logical indicating whether missing values should be removed.
#' @name parallel



#' @rdname parallel
#' @export
psum <- function(..., na.rm = TRUE) {
  purrr::pmap_dbl(list(...), \(...) {
    sum(..., na.rm = na.rm)
  })
}



#' @rdname parallel
#' @export
pprod <- function(..., na.rm = TRUE) {
  purrr::pmap_dbl(list(...), \(...) {
    prod(..., na.rm = na.rm)
  })
}



#' @rdname parallel
#' @export
pall <- function(..., na.rm = TRUE) {
  purrr::pmap_lgl(list(...), \(...) {
    all(..., na.rm = na.rm)
  })
}



#' @rdname parallel
#' @export
pany <- function(..., na.rm = TRUE) {
  purrr::pmap_lgl(list(...), \(...) {
    any(..., na.rm = na.rm)
  })
}
