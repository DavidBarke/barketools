% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3_utils.R
\name{s3_list_files}
\alias{s3_list_files}
\title{List Files on S3}
\usage{
s3_list_files(
  prefix = NULL,
  bucket = "gcpd",
  include_folder = FALSE,
  max = 1000,
  value = c("vector", "df"),
  delimiter = NULL
)
}
\arguments{
\item{prefix}{Common prefix to all files.}

\item{bucket}{S3 bucket.}

\item{include_folder}{If `TRUE`, also include S3 keys referring to folders.}

\item{max}{Maximum number of results to return.}

\item{value}{See `Value`.}

\item{delimiter}{Character string used to group keys. Read the AWS docs for
more details.}
}
\value{
If `value == "vector"`, a character vector of S3 keys. If
`value == "df"` a data frame of S3 keys.
}
\description{
List Files on S3
}