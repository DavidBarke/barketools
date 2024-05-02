# Ported from GCPD/pygcpd/s3/inventory_utils.py

#' Get Most Recent Inventory Date
#'
#' @param bucket S3 bucket.
#'
#' @export
get_most_recent_inventory_date <- function(bucket = "gcpd") {
  inventory_keys <- s3_list_subfolders(
    prefix = glue::glue(
      "inventory-reports/{bucket}/inventory/",
      bucket = bucket
    ),
    bucket = bucket
  )

  inventory_keys |>
    extract_inventory_date() |>
    lubridate::as_date() |>
    max(na.rm = TRUE)
}



extract_inventory_date <- function(key) {
  key |>
    stringr::str_extract("(\\d{4}-\\d{2}-\\d{2})", group = 1)
}



#' Read Inventory
#'
#' @param date Date in format YYYY-MM-DD.
#' @param bucket S3 bucket.
#'
#' @export
read_inventory <- function(date = get_most_recent_inventory_date(), bucket) {
  manifest <- aws.s3::get_object(
    object = glue::glue(
      "inventory-reports/{bucket}/inventory/{date}T01-00Z/manifest.json",
      bucket = bucket,
      date = date
    ),
    bucket = bucket,
    as = "raw"
  ) |>
    rawToChar() |>
    jsonlite::fromJSON()

  tf <- tempfile(fileext = ".csv.gz")
  inventory <- manifest$files$key |> purrr::map(\(key) {
    aws.s3::save_object(
      object = key,
      bucket = bucket,
      file = tf
    )

    arrow::read_csv_arrow(
      tf,
      col_names = c("bucket", "key")
    )
  }, .progress = TRUE) |>
    purrr::list_rbind()
}
