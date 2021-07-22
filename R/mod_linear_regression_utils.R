#' rl_model
#' 
#' @description generates a linear regression model.
#'
#' @param data dataframe
#' @param variable.pred the name of the variable to be predicted.
#' 
#' @seealso \code{\link[stats]{lm}}
#'
#' @export
rl_model <- function(data, variable.pred){
  if(!is.null(variable.pred) && !is.null(data)){
    return(lm(formula = paste0(variable.pred,"~."), data = data))
  }
  return(NULL)
}

#' rl_prediction
#' 
#' @description generates the prediction of the linear regression model.
#'
#' @param model a linear regression model(lm).
#' @param test.data dataframe.
#'
#' @seealso \code{\link[stats]{predict}}
#'
#' @export
#' 
rl_prediction <- function(model, test.data) {
  return(predict(model,test.data))
  #return(paste0(pred.var, " <- predict(",model.var,", ",data,")"))
}

#' rl_coeff
#' 
#' @description get the information of the coefficients of the linear regression model
#'
#' @param modelo linear regression model
#'
#' @export
#'
rl_coeff <- function(modelo){
  if(!is.null(modelo)){
    summ <- summary(modelo)
    df.rl <- as.data.frame(summ$coefficients)
    df.rl <- cbind(df.rl,  Importance = symnum(summ$coefficients[,4], corr = FALSE, na = FALSE,
                                               cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                               symbols = c('***', '**', '*', '.', ' ')))
    df.rl <- as.data.frame(df.rl)
    r2 <- summ$r.square
    
    return(list(df.rl = df.rl, r2 = r2))
  }
  return(NULL)
  
  # paste0("summ <- summary(",model.var,")\n",
  #        "df.rl <- as.data.frame(summ$coefficients)\n",
  #        "df.rl <- cbind(df.rl,  Importance = symnum(summ$coefficients[,4], corr = FALSE, na = FALSE, 
  #                                          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                                          symbols = c('***', '**', '*', '.', ' ')))\n",
  #        "df.rl <- as.data.frame(df.rl)\n",
  #        "r2    <- summ$r.square\n")
}

#------------------------------------CODE---------------------------------------

codeRl <- function(variable.predecir){
  return(paste0("rl_model(data, '",variable.predecir,"')"))
}

codeRlCoef <- function(nombreModelo = "modelo.rl"){
  return(paste0("information <- rl_coeff(",nombreModelo,")\n",
                "information$df.rl[,c(1,4)]"))
}

codeRlPred <- function(nombreModelo){
  return(paste0("rl_prediction(model = ", nombreModelo, " test.data"))
}

codeRlIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.rl)"))
}