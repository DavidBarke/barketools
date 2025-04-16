#' Save Figure
#'
#' Interface to [ggplot2::ggsave].
#'
#' @param p Plot to save.
#' @param file File name to create on disk. Make sure to not include the file
#' type, i.e., if you want to create a PDF do not put ".pdf" at the end.
#' @param file_formats A character vector of file formats supported by
#' [ggplot2::ggsave]. The figure is saved for each of these file formats.
#' @param width,height Plot size in units expressed by the *units* argument. If
#' not supplied, uses the size of the current graphics device.
#'
#' @importFrom purrr walk
#' @importFrom ggplot2 ggsave
#'
#' @export
save_figure <- function(
    p, file, file_formats, width = 7, height = 5, ...
) {
  purrr::walk(file_formats, function(file_format) {
    file_name <- paste0(file, ".", file_format)
    ggplot2::ggsave(
      file_name, p, width = width, height = height, ...
    )
  })

  p
}
