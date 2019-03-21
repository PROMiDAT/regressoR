
#' general.indices
#'
#' @description calculates indices to measure accuracy of a model 
#'
#' @param real the real values in traning-testing.
#' @param prediccion the prediction values in traning-testing.
#'
#' @return a list with Correlation, Relative Error, Mean Absolute Error, Root Mean Square Error
#' @export
#'
#' @examples
#' real <- rnorm(45)
#' prediction <- rnorm(45)
#' model <- "KNN"
#' general.indices(real, prediction)
#' 
general.indices <- function(real, prediccion) {
  RMSE <- sqrt(sum((real - prediccion) ^ 2) / length(prediccion))
  MAE  <- sum(abs(real - prediccion)) / length(prediccion)
  RE   <- sum(abs(real - prediccion)) / sum(abs(real))
  COR  <- as.numeric(cor(real, prediccion))
  COR  <- ifelse(is.na(COR), 0 , COR)
  return(list(Raiz.Error.Cuadratico = RMSE,
              Error.Absoluto = MAE,
              Error.Relativo = RE,
              Correlacion = COR))
}

#' combine.names
#'
#' @param x a vector to combine with y. The combination is grouped by this parameter.
#' @param y a vector to combine with x.
#' @param sep a string with the separator characters.
#'
#' @return a vector with the combination of x and y.
#' @export
#'
#' @examples
#' x = c("A", "B", "C")
#' y = c("1", "2", "3")
#' combine.names(x, y)
#' 
combine.names <- function(x, y, sep = "."){
  unlist(lapply(y, function(y1)lapply(x, function(x1)paste0(x1,sep,y1))))
}

#' colnames.empty
#'
#' @param data the dataset
#'
#' @return a vector with the names of the columns or an empty string if the data is NULL
#' @export
#'
#' @examples
#' colnames.empty(iris)
#' colnames.empty(NULL)
#' 
colnames.empty <- function(data){
  res <- colnames(data)
  if(is.null(res))
    return("")
  return(res)
}

#' var.numerical
#'
#' @description gets only the numerical columns
#'
#' @param data the dataset
#'
#' @return a vector with the names of the numerical columns
#' @export
#'
#' @examples
#' var.numerical(iris)
#' 
var.numerical <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#' var.categorical
#'
#' @description gets only the categorical columns
#'
#' @param data the dataset
#'
#' @return a vector with the names of the categorical columns
#' @export
#'
#' @examples
#' var.categorical(iris)
#' 
var.categorical <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}


#' summary_indices
#'
#' @description resume una variable retornando el mínimo, el primer cuartil, el tercer cuartil y el valor máximo.
#'
#' @param data a numeric vector. 
#'
#' @return a list with the summary.
#' @export
#'
#' @examples
#' summary_indices(iris$Sepal.Length)
summary_indices <- function(data){
  list("Min" = min(data),
       "1Q"  = quantile(data, prob=c(0.25)),
       "3Q"  = quantile(data, prob=c(0.75)),
       "Max" = max(data))
}

#' disjunctive.data
#' 
#' @description Convert the columns selected to disjunctive
#'
#' @param data the dataset to be converted
#' @param vars a vector with the name of columns
#'
#' @return a dataset
#' @export
#'
#' @examples
#' disjunctive.data(iris, "Species")
disjunctive.data <- function(data, vars){
  if(is.null(data)) return(NULL)
  cualitativas <- base::subset(data, select = colnames(data) %in% c(vars))
  data <- data[, !colnames(data) %in% vars]
  for (variable in colnames(cualitativas)) {
    for (categoria in unique(cualitativas[, variable])) {
      nueva.var <- as.numeric(cualitativas[, variable] == categoria)
      data <- cbind(data, nueva.var)
      colnames(data)[length(colnames(data))] <- paste0(variable, '.', categoria)
    }
  }
  return(data)
}
