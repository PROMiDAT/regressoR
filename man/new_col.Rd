% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_manipulation.R
\name{new_col}
\alias{new_col}
\title{new_col}
\usage{
new_col(data, name = "new_", values = NA)
}
\arguments{
\item{data}{the data.frame to join with the new column.}

\item{name}{the name of the new column.}

\item{values}{the values of the new column.}
}
\description{
creates a new column.
}
\examples{
new_col(iris)
new_col(iris, "var1", c(1,2,3))

}
