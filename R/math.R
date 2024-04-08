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
