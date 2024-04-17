#' @export
data_source_s3_path <- function(
    src = c("compustat", "pitchbook", "patents_view", "wciklink", "geo"),
    release, table, type = c("raw", "processed")
) {
  f <- switch (src,
    "compustat" = compustat_s3_path,
    "pitchbook" = pitchbook_s3_path,
    "patents_view" = patents_view_s3_path,
    "wciklink" = wciklink_s3_path,
    "geo" = geo_s3_path
  )

  f(
    release = release,
    type = type,
    table = table
  )
}



#' @export
geo_s3_path <- function(
    release = "01_24",
    type = "processed",
    table = c("countries", "states")
) {
  type <- match.arg(type)
  table <- match.arg(table)

  glue::glue(
    "s3://gcpd/data-sources/geo/release={release}/type={type}/table={table}",
    release = release,
    type = type,
    table = table
  )
}



#' @export
compustat_s3_path <- function(
    release = "09_23",
    table = "company",
    type = c("raw", "processed")
) {
  type <- match.arg(type)

  glue::glue(
    "s3://gcpd/data-sources/compustat/release={release}/type={type}/table={table}",
    release = release,
    type = type,
    table = table
  )
}



#' @export
pitchbook_s3_path <- function(
    release = "2021",
    type = c("raw", "processed"),
    table
) {
  type <- match.arg(type)

  glue::glue(
    "s3://gcpd/data-sources/pitchbook/release={release}/type={type}/table={table}",
    release = release,
    table = table
  )
}



#' @export
patents_view_s3_path <- function(
    release = "09_23",
    type = c("raw", "processed"),
    table
) {
  type <- match.arg(type)

  glue::glue(
    "s3://gcpd/data-sources/patents_view/release={release}/type={type}/table={table}",
    type = type,
    release = release,
    table = table
  )
}



#' @export
wciklink_s3_path <- function(
    release = "09_23",
    type = c("raw", "processed"),
    table = "wciklink_gvkey"
) {
  type <- match.arg(type)

  glue::glue(
    "s3://gcpd/data-sources/wciklink/release={release}/type={type}/table={table}",
    release = release,
    type = type,
    table = table
  )
}



#' @export
names_s3_path <- function(release, table) {
  glue::glue(
    "s3://gcpd/data-sources/names/release={release}/table={table}",
    release = release,
    table = table
  )
}



#' @export
upload_task_s3_path <- function(full = TRUE) {
  if (full) "s3://gcpd/upload-task" else "upload-task"
}
