#' general_indices
#'
#' @description calculates indices to measure accuracy of a model.
#'
#' @param real the real values in traning-testing.
#' @param prediccion the prediction values in traning-testing.
#'
#' @return a list with the Correlation, Relative Error, Mean Absolute Error and Root Mean Square Error.
#' @export
#'
#' @examples
#' real <- rnorm(45)
#' prediction <- rnorm(45)
#' model <- "KNN"
#' general_indices(real, prediction)
#' 
general_indices <- function(real, prediccion) {
  RMSE <- sqrt(sum((real - prediccion) ^ 2) / length(prediccion))
  MAE  <- sum(abs(real - prediccion)) / length(prediccion)
  RE   <- paste0(as.character(round(sum(abs(real - prediccion)) / sum(abs(real)) * 100, 3)), "%")
  desvStand <- sd(prediccion)
  COR  <- ifelse(dplyr::near(desvStand,0), 0, as.numeric(cor(real, prediccion)))
  COR  <- ifelse(is.na(COR), 0 , COR)
  return(list(Raiz.Error.Cuadratico = RMSE,
              Error.Absoluto = MAE,
              Error.Relativo = RE,
              Correlacion = COR))
}

#' summary_indices
#'
#' @description summarizes a variable by returning the minimum, first quartile, third quartile and maximum value.
#'
#' @param data a numeric vector. 
#'
#' @export
#'
#' @examples
#' summary_indices(iris$Sepal.Length)
#' 
summary_indices <- function(data){
  list("Min" = min(data),
       "1Q"  = quantile(data, prob=c(0.25)),
       "3Q"  = quantile(data, prob=c(0.75)),
       "Max" = max(data))
}