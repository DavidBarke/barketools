s3_list_files <- function(
  prefix = NULL, bucket = "gcpd", include_folder = FALSE, max = 1e3,
  value = c("vector", "df"), delimiter = NULL
) {
  value <- match.arg(value)

  files <- aws.s3::get_bucket_df(
    bucket = bucket,
    prefix = prefix,
    max = max,
    delimiter = delimiter
  )

  df <- if (include_folder) {
    files
  } else {
    files |>
      dplyr::filter(!stringr::str_detect(Key, "/$"))
  } |>
    tibble::as_tibble()

  if (value == "vector") df$Key else df
}



s3_list_subfolders <- function(
    prefix = NULL, bucket = "gcpd", max = 1e3, subfolder_only = FALSE
) {
  sf <- aws.s3::get_bucket(
    prefix = prefix,
    bucket = bucket,
    max = max,
    delimiter = "/"
  ) |>
    attr("CommonPrefixes")

  if (subfolder_only) {
    sf |>
      stringr::str_remove(prefix) |>
      stringr::str_remove("/$")
  } else sf
}
