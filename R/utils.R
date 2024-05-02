#' @export
index_split <- function(x, n) {
  if (length(x) <= n) return(purrr::map(x, identity))
  purrr::map(seq_len(n), \(i) {
    x[seq(i, length(x), by = n)]
  })
}



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
