% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repair_patents_view_filing_dates.R
\name{repair_pv_filing_dates}
\alias{repair_pv_filing_dates}
\title{Repair PatentsView Filing Dates}
\usage{
repair_pv_filing_dates(filing_dates)
}
\arguments{
\item{filing_dates}{A vector of filing dates.}
}
\description{
This function repairs filing dates of patents in PatentsView. Consider for
example, patent \href{https://patents.google.com/patent/US4469216A}{US4469216}
which has a recorded filing date of \verb{Apr. 5, 8198}. The corrected filing
date is \verb{Apr. 5, 1982}. All corrected filing dates were manually confirmed.
}
