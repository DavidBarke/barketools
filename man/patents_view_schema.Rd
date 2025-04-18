% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patents_view_schema.R
\docType{data}
\name{patents_view_schema}
\alias{patents_view_schema}
\title{PatentsView Schema}
\format{
A data frame with 214 rows and 7 columns:
\describe{
\item{table}{Table name, e.g., \code{"g_applicant_not_disambiguated"}}
\item{column}{Column name, e.g., \code{"patent_id"}}
\item{column_type}{SQL column type, e.g., \code{"varchar(36)"}}
\item{definition}{Column definition}
\item{example}{Example value}
\item{years_present}{Years for which data is present}
\item{r_column_type}{\code{"character"}, \code{"integer"}, or \code{"date"}}
}
}
\source{
https://patentsview.org/download/data-download-dictionary
}
\usage{
patents_view_schema
}
\description{
This dataset is a precomputed version of \code{\link[=patents_view_schema]{patents_view_schema()}}.
}
\keyword{datasets}
