#' @importFrom purrr walk
#' @importFrom ggplot2 ggsave
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
