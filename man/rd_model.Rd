% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_dimension_reduction_utils.R
\name{rd_model}
\alias{rd_model}
\title{rd_model}
\usage{
rd_model(data, variable.pred, mode = 0, scale = TRUE)
}
\arguments{
\item{data}{dataframe}

\item{variable.pred}{the name of the variable to be predicted.}

\item{mode}{the method of dimension reduction is defined as mode=1 is the MCP, and mode=0 the ACP.}

\item{scale}{the scale parameter of the model.}
}
\description{
generates a dimension reduction model.
}
\seealso{
\code{\link[pls]{pcr}}, \code{\link[pls]{plsr}}
}
