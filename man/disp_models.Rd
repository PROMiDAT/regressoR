% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_l_regression_utils.R
\name{disp_models}
\alias{disp_models}
\title{disp_models}
\usage{
disp_models(prediction, model_name, var_pred)
}
\arguments{
\item{prediction}{the name of the prediction object.}

\item{model_name}{the name of the model.}

\item{var_pred}{the name of the variable to be predicted.}
}
\description{
this function generates the call code of the scatter function.
}
\examples{
disp_models("prediction.knn", "KNN", "Species")

}
