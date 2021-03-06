% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_manipulation.R
\name{remove_report_elem}
\alias{remove_report_elem}
\title{remove_report_elem}
\usage{
remove_report_elem(id)
}
\arguments{
\item{id}{a string with the key of what is removed in the report.}
}
\description{
removes an element from the report according to its key in the current section.
}
\examples{
new_report(iris, 'iris')
insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
get_report()
remove_report_elem('1_part')
get_report()
clean_report()

}
