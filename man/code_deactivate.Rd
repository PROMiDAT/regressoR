% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_generate.R
\name{code_deactivate}
\alias{code_deactivate}
\title{code_deactivate}
\usage{
code_deactivate(variables, d = "datos")
}
\arguments{
\item{variables}{the name of the variables to be deactivated.}

\item{d}{the name of the current data.}
}
\description{
creates the code that deactivates the selected variables of the data.
}
\examples{
iris2 <- iris
x <- code_deactivate('Species', 'iris2')
exe(x)
head(iris2)

}
