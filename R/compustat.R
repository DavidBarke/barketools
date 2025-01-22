#' @export
pad_gvkey <- function(gvkey) {
  if (!all(stringr::str_detect(gvkey, "^\\d{4,6}$"))) {
    cli::cli_abort("'gvkey' has to consist of 4 to 6 digits.")
  }
  stringr::str_pad(gvkey, 6, "left", "0")
}
