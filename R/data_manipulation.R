
#' general_indices
#'
#' @description calculates indices to measure accuracy of a model.
#'
#' @param real the real values in traning-testing.
#' @param prediccion the prediction values in traning-testing.
#'
#' @return a list with Correlation, Relative Error, Mean Absolute Error, Root Mean Square Error.
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
  RE   <- sum(abs(real - prediccion)) / sum(abs(real))
  COR  <- as.numeric(cor(real, prediccion))
  COR  <- ifelse(is.na(COR), 0 , COR)
  return(list(Raiz.Error.Cuadratico = RMSE,
              Error.Absoluto = MAE,
              Error.Relativo = RE,
              Correlacion = COR))
}

#' combine_names
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
#' combine_names(x, y)
#' 
combine_names <- function(x, y, sep = "."){
  unlist(lapply(y, function(y1)lapply(x, function(x1)paste0(x1,sep,y1))))
}

#' colnames_empty
#'
#' @param data the dataset.
#'
#' @return a vector with the names of the columns or an empty string if the data is NULL.
#' @export
#'
#' @examples
#' colnames_empty(iris)
#' colnames_empty(NULL)
#' 
colnames_empty <- function(data){
  res <- colnames(data)
  if(is.null(res))
    return("")
  return(res)
}

#' var_numerical
#'
#' @description gets only the numerical columns.
#'
#' @param data the dataset.
#'
#' @return a vector with the names of the numerical columns.
#' @export
#'
#' @examples
#' var_numerical(iris)
#' 
var_numerical <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#' var_categorical
#'
#' @description gets only the categorical columns.
#'
#' @param data the dataset.
#'
#' @return a vector with the names of the categorical columns.
#' @export
#'
#' @examples
#' var_categorical(iris)
#' 
var_categorical <- function(data){
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
#' 
summary_indices <- function(data){
  list("Min" = min(data),
       "1Q"  = quantile(data, prob=c(0.25)),
       "3Q"  = quantile(data, prob=c(0.75)),
       "Max" = max(data))
}

#' disjunctive_data
#' 
#' @description Convert the columns selected to disjunctive.
#'
#' @param data the dataset to be converted.
#' @param vars a vector with the name of columns.
#'
#' @return a dataset
#' @export
#'
#' @examples
#' disjunctive_data(iris, "Species")
#' 
disjunctive_data <- function(data, vars){
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

#' comparative_table
#'
#' @description creates the comparison table.
#'
#' @param sel the selection of the models to be shown.
#' @param indices the values to be shown.
#' @param language the language to choose. It can be "es" or "en".
#' 
#' @export
#'
#' @examples
#' models <- list('knnl-mode1' = list(0.11,0.22,0.33,0.44),
#'                'dtl-mode2'  = list(0.12,0.23,0.34,0.45),
#'                'rfl-mode1'  = list(0.51,0.42,0.13,0.24))
#' sel <- c("K Vecinos Más Cercanos-mode1", "Bosques Aleatorios-mode1")
#' comparative_table(sel, models)
#' 
comparative_table <- function(sel, indices, language = "es") {
  tryCatch({
    nombres <- models_mode(indices, language)
    
    if(nombres[1] == "---X---") {
      return(data.frame())
    }
    resp <- do.call(rbind, indices)
    rownames(resp) <- nombres
    colnames(resp) <- c(translate("RMSE", language), translate("MAE", language),
                        translate("ER", language)  , translate("correlacion", language))
    resp <- as.data.frame(resp)
    resp[] <- lapply(resp, as.numeric)
    resp <- round(resp, 4)
    resp <- resp[nombres %in% sel,]
    return(resp)
    
  }, error = function(e){
    return(data.frame())
  })
}

############################ NP

#' validate_pn_data
#' 
#' @description Verify that a data.frame has the same columns with the same types.
#'
#' @param x a data.frame with criteria to compare.
#' @param y a data.frame to be comprared.
#' @param var.pred a vector with the names of variables to be excluded from the comparison.
#' @param language the language to choose. It can be "es" or "en".
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' validate_pn_data(iris, cars)
#' validate_pn_data(iris, iris)
#' x <- iris
#' x$Species <- as.numeric(x$Species)
#' validate_pn_data(iris, x)
#' }
validate_pn_data <- function(x, y, var.pred = "", language = "es"){
  nombres <- colnames(x)
  selec <- -which(nombres == var.pred)
  if(length(selec) > 0){
    nombres  <- nombres[]
  }
  nombres.prueba <- colnames(y)
  
  if(any(!(nombres.prueba %in% nombres))){
    stop(translate("NoTamColum"), call. = FALSE) 
  }
  
  tipos <- unlist(lapply(x[,nombres, drop = FALSE], class))
  tipos.prueba <- unlist(lapply(y[,nombres, drop = FALSE], class))
  
  if(any(tipos != tipos.prueba)){
    stop(translate("NoTamColum"),call. = FALSE)
  }
}

#' new_col
#' 
#' @description creates a new column.
#'
#' @param data the data.frame to join with the new column.
#' @param name the name of the new column.
#' @param values the values of the new column.
#'
#' @export
#'
#' @examples
#' new_col(iris)
#' new_col(iris, "var1", c(1,2,3))
#' 
new_col <- function(data, name = "new_", values = NA){
  data[,name] <- values
  return(data)
}

#' fisher_calc
#' 
#' @description calculate the fisher skewness.
#'
#' @param x a vector with the data to make the calculation.
#' @param na.rm a logical value indicating whether the NAs have to be eliminated.
#'
#' @export
#'
#' @examples
#' fisher_calc(iris$Petal.Length)
#' 
fisher_calc <- function (x, na.rm = FALSE) {
  if (na.rm){
    x <- x[!is.na(x)]
  }
  return(sum((x - mean(x))^3/sd(x)^3)/length(x))
}

