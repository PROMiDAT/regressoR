% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_generate.R
\name{boosting_model}
\alias{boosting_model}
\title{boosting_model}
\usage{
boosting_model(
  data = "datos.aprendizaje",
  variable.pred = NULL,
  model.var = "modelo.boosting",
  n.trees = 50,
  distribution = "gaussian",
  shrinkage = 0.1
)
}
\arguments{
\item{data}{the name of the learning data.}

\item{variable.pred}{the name of the variable to be predicted.}

\item{model.var}{the name of the variable that stores the resulting model.}

\item{n.trees}{the n.trees parameter of the model.}

\item{distribution}{the distribution parameter of the model.}

\item{shrinkage}{the shrinkage parameter of the model.}
}
\description{
generates the code to create the boosting model.
}
\examples{
library(gbm)
library(dplyr)

x <- boosting_model('iris', 'Petal.Length')
exe(x)
print(modelo.boosting)

}
\seealso{
\code{\link[gbm]{gbm}}
}
