% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3_utils.R
\name{s3_read_xlsx}
\alias{s3_read_xlsx}
\title{Read XLSX file from S3}
\usage{
s3_read_xlsx(bucket, key)
}
\arguments{
\item{bucket}{S3 bucket.}

\item{key}{S3 key of XLSX file.}
}
\value{
A tibble.
}
\description{
Read XLSX file from S3
}
