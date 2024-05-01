#' @export
index_split <- function(x, n) {
  if (length(x) <= n) return(purrr::map(x, identity))
  purrr::map(seq_len(n), \(i) {
    x[seq(i, length(x), by = n)]
  })
}
