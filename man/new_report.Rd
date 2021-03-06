% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_manipulation.R
\name{new_report}
\alias{new_report}
\title{new_report}
\usage{
new_report(data, name = "")
}
\arguments{
\item{data}{the data that is stored in the report list}

\item{name}{the name of the stored data}
}
\description{
creates a new report section within the list. All new reports section store data and data names as headers.
}
\examples{
new_report(iris, "iris")
get_report()
clean_report()

}
