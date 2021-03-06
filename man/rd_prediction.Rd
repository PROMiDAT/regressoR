% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_generate.R
\name{rd_prediction}
\alias{rd_prediction}
\title{rd_prediction}
\usage{
rd_prediction(
  data = "datos.prueba",
  model.var = "modelo.svm",
  pred.var = "prediccion.rd",
  n.comp = "n.comp.rd",
  ncomp = NULL
)
}
\arguments{
\item{data}{the name of the test data.}

\item{model.var}{the name of the variable that stores the resulting model.}

\item{pred.var}{the name of the variable that stores the resulting prediction.}

\item{n.comp}{the name of the variable that stores the optimum number of components.}

\item{ncomp}{a numerical value in case you don't want to use the optimum number of components.}
}
\description{
generates the code to create the prediction of the dimension reduction model.
}
\examples{
library(pls)

x <- rd_model('iris', 'Petal.Length')
exe(x)
print(modelo.rd)

x <- rd_prediction('iris', 'modelo.rd', 'my_prediction')
exe(x)
print(my_prediction)

}
