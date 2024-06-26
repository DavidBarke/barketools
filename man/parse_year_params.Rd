% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/year_rmd_params.R
\name{parse_year_params}
\alias{parse_year_params}
\title{Parse RMarkdown Year Parameters}
\usage{
parse_year_params(params)
}
\arguments{
\item{params}{The `params` object made available by RMarkdown. It should
have the fields `years` (a comma-separated string of years), `year_start`
or `year_end`. If `years` is different from `NULL` (`~` in YAML), `years`
takes precedence over `year_start` and `year_end`.}
}
\value{
An integer vector of years.
}
\description{
This is a convenience function to determine the set of years for which the
RMarkdown script has to be run. This is useful when the RMarkdown script
independently executes a function for each year.
}
