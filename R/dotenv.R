#' @export
env_file <- function(env_file = NULL) {
  if (is.null(env_file)) {
    file.path(devtools::package_file(), '.env')
  } else env_file
}
