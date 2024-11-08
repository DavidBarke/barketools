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
#'
#' @export
print_nrow <- function(df) {
  n <- nrow(df) |>
    format(big.mark = ",", scientific = FALSE)
  cli::cli_alert_info("Number of rows: {n}")
  invisible(df)
}



#' Print object if condition holds
#'
#' @param x Object to be printed conditionally.
#' @param cond Logical. If `TRUE`, `x` is printed.
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
