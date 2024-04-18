#' @export
pad_gvkey <- function(gvkey) {
  str_pad(gvkey, 6, "left", "0")
}
