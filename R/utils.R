#' Split a vector into modulus batches
#'
#' @param x A vector.
#' @param n An integer.
#'
#' @returns A list with at most `n` elements. The `i`-th element is a vector
#' containing all the elements of `x` belonging to the same equivalence class
#' with respect to the `modulus` operation. That means \eqn{x_1} and \eqn{x_2}
#' are both elements of `out[i]` if eqn{x_1 mod n = x_2 mod n = i}.
#'
#' @export
index_split <- function(x, n) {
  if (length(x) <= n) return(purrr::map(x, identity))
  purrr::map(seq_len(n), \(i) {
    x[seq(i, length(x), by = n)]
  })
}



#' Split a vector into sequential batches
#'
#' @param x A vector.
#' @param batch_size An integer.
#'
#' @returns A list where each element contains `batch_size` elements of `x`. The
#' last element of the list contains at most `batch_size` elements of `x`.
#'
#' @export
batch_vector <- function(x, batch_size) {
  n_x <- length(x)
  n_batches <- ceiling(n_x / batch_size)
  purrr::map(seq_len(n_batches), \(batch) {
    batch_start <- (batch - 1) * batch_size + 1
    batch_end <- min(batch * batch_size, n_x)
    x[batch_start:batch_end]
  })
}



#' Print number of rows
#'
#' @param df Data frame.
#' @param f Function. Number of rows of `f(df)` is printed.
#'
#' @returns The unaltered data frame `df`.
#'
#' @export
print_nrow <- function(df, f = identity) {
  n <- nrow(f(df)) |>
    format(big.mark = ",", scientific = FALSE)
  cli::cli_alert_info("Number of rows: {n}")
  invisible(df)
}



#' Print object if condition holds
#'
#' @param x Object to be printed conditionally.
#' @param cond Logical. If `TRUE`, `x` is printed.
#'
#' @returns The unaltered object `x`.
#'
#' @export
print_if <- function(x, cond) {
  if (cond) print(x)
  x
}



#' Apply function if condition holds
#'
#' @param x Object to which function is applied conditionally.
#' @param cond Logical.
#' @param f Function that takes one argument `x`.
#'
#' @returns If `cond` `f(x)`, else `x`.
#'
#' @export
apply_if <- function(x, cond, f) {
  if (cond) f(x) else x
}



#' Get Current Date
#'
#' Like [Sys.Date()] but with `_` instead of `-`.
#'
#' @export
sys_date <- function() {
  Sys.Date() |>
    gsub(
      pattern = "-",
      replacement = "_",
      x = _
    )
}



#' Extract Unique Elements (Without NA)
#'
#' Like [unique] but `NA`s are dropped.
#'
#' @inheritParams base::unique
#'
#' @export
unique_non_NA <- function(x, incomparables = FALSE, ...) {
  unique(x, incomparables = incomparables, ...) |>
    setdiff(NA)
}



#' Concatenate Strings
#'
#' Like [paste] but `NA`s are dropped.
#'
#' @inheritParams base::paste
#' @param NA_if_all_NA If `TRUE`, return `NA` if `...` only contains `NA`. If
#' `FALSE`, return `character(0)`.
#'
#' @name pasteNA
#'
#' @export
paste_NA <- function(
  ..., sep = " ", collapse = NULL, recycle0 = FALSE, NA_if_all_NA = TRUE
) {
  args <- list(...)
  args <- args[!is.na(args)]
  if (NA_if_all_NA && all(is.na(args))) return(NA)
  do.call(paste, c(args, sep = sep, collapse = collapse, recycle0 = recycle0))
}



#' @rdname pasteNA
#' @export
paste0_NA <- function(
  ..., collapse = NULL, recycle0 = FALSE, NA_if_all_NA = TRUE
) {
  paste_NA(
    ...,
    sep = "",
    collapse = collapse,
    recycle0 = recycle0,
    NA_if_all_NA = NA_if_all_NA
  )
}
