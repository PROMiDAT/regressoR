#' rlr_model
#' 
#' @description generates a penalized regression model.
#'
#' @param data dataframe
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param alpha the alpha parameter of the model.
#' @param standardize the standardize parameter of the model.
#'
#' @seealso \code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}
#'
#' @export
rlr_model <- function(data, variable.pred,alpha = 0, standardize = TRUE){
  if(!is.null(variable.pred) && !is.null(data)){
    form <- formula(paste0(variable.pred,"~."))
    x <- model.matrix(form,data)[, -1]
    y <- data[,variable.pred]
    modelo.rlr <- cv.glmnet(x, y, standardize = standardize, alpha = alpha)
    
    return(modelo.rlr)
  }
  return(NULL)
  
  # return(paste0("x <- model.matrix(`",variable.pred,"`~., ",data,")[, -1]\n",
  #               "y <- ",data,"[, '",variable.pred,"']\n",
  #               model.var," <- cv.glmnet(x, y, standardize = ",standardize,", alpha = ",alpha,")"))
}

#' coef_lambda
#' 
#' @description generates the code to print the penalized regression coefficients.
#'
#' @param data dataframe
#' @param variable.pred the name of the variable to be predicted.
#' @param model a penalized regression model(cv.glmnet).
#' @param log.lambda numerical. Logarithm of lambda in case you don't want to use the optimal lambda.
#'
#' @export
#' 
coef_lambda <- function(data , variable.pred, model, log.lambda = NULL){
  if(!is.null(variable.pred) && !is.null(data) && !is.null(model)){
    
    form <- formula(paste0(variable.pred,"~."))
    x <- model.matrix(form,data)[, -1]
    y <- data[,variable.pred]
    print(log.lambda)
    lambda <- ifelse(is.null(log.lambda), model$lambda.min, exp(log.lambda))
    print(coefficients(model, s = lambda))
    p <- predict(model, s = lambda, type = 'coefficients', exact = TRUE, x = x, y = y)
    print("PP")
    return(predict(model, s = lambda, type = 'coefficients', exact = TRUE, x = x, y = y))
  }
  return(NULL)

  # paste0("x <- model.matrix(`",variable.pred,"`~., ",data,")[, -1]\n",
  #        "y <- ",data,"[, '",variable.pred,"']\n",
  #        "predict(",model.var,", s = ",lambda,", type = 'coefficients', exact = TRUE, x = x, y = y)")
}


#' rlr_prediction
#' 
#' @description generates the code to create the prediction of the penalized regression model.
#'
#' @param learning.data dataframe.
#' @param test.data dataframe.
#' @param variable.pred the name of the variable to be predicted.
#' @param model a penalized regression model(cv.glmnet).
#' @param log.lambda numerical. Logarithm of lambda in case you don't want to use the optimal lambda.
#'
#' @export
#'
rlr_prediction <- function(learning.data, test.data, variable.pred, model, log.lambda = NULL) {
  if(!is.null(learning.data) && !is.null(test.data) && !is.null(variable.pred) && !is.null(model)){
    lambda <- ifelse(is.null(log.lambda), model$lambda.min, exp(log.lambda))
    form <- formula(paste0(variable.pred,"~."))
    x <- model.matrix(form,learning.data)[, -1]
    y <- learning.data[,variable.pred]
    prueba <- test.data
    prueba[, variable.pred] <- 0
    prueba <- model.matrix(form, prueba)[, -1]
    return(predict(model,newx = prueba, s = lambda, exact = TRUE, x = x, y = y))
  }
  return(NULL)
  
  # paste0("x <- model.matrix(`",variable.pred,"`~., ",data.a,")[, -1]\n",
  #        "y <- ",data.a,"[, '",variable.pred,"']\n",
  #        "prueba <- ",data.p,"\n",
  #        "prueba[, '",variable.pred,"'] <- 0\n",
  #        "prueba <- model.matrix(`",variable.pred,"`~., prueba)[, -1]\n",
  #        pred.var," <- predict(",model.var,",newx = prueba,",
  #        "s = ",lambda,", exact = TRUE, x = x, y = y)")
}

#' rlr_type
#' 
#' @description returns the name of the penalty according to the alpha.
#'
#' @param alpha_rlr the penalty is defined as alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
#' 
#' @seealso \code{\link[glmnet]{glmnet}}
#'
#' @export
#'
#' @examples
#' rlr_type(1)
#' rlr_type(0)
#' 
rlr_type <- function(alpha_rlr = 0){
  alpha_rlr <- ifelse(is.null(alpha_rlr), 0, alpha_rlr)
  ifelse(alpha_rlr == 0, "ridge", "lasso")
}


#' e_posib_lambda
#' 
#' @description Graph a cv.glmnet model
#'
#' @param cv.glm a cv.glmnet model.
#' @param log.lambda number that specifies the logarithm of the selected lambda
#' @param titles labels on the chart
#'
#' @seealso \code{\link[glmnet]{cv.glmnet}}
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' 
#' @export
#' 
e_posib_lambda <- function(cv.glm, log.lambda = NULL, titles = c("Error Cuadrático Medio","Curva Inferior",
                                                                 "Curva Superior","Seleccionado",
                                                                 "Coeficientes Distintos de Cero")){
  x  <- log(cv.glm$lambda)
  y  <- cv.glm$cvm
  x1 <- x[cv.glm$index[[1]]]
  x2 <- x[cv.glm$index[[2]]]
  upper <- cv.glm$cvup
  lower <- cv.glm$cvlo
  nzero  <- cv.glm$nzero
  data.lambda <- data.frame(x, y, upper, lower, nzero)
  
  grafico  <- data.lambda %>%
    e_charts(x) %>%
    e_scatter(y, symbol_size = 11, color = "red", 
              tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                "return(params.marker + '<br/>' + ",
                                                                "'<b>Log(lambda): </b>' + ",
                                                                "Number.parseFloat(params.value[0]).toFixed(6) + ",
                                                                "'<br/><b>", titles[1], ": </b>' + ",
                                                                "Number.parseFloat(params.value[1]).toFixed(6))}")))) %>%
    e_error_bar(lower, upper,
                tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                  "return('<b>", titles[2], ": </b>' + ",
                                                                  "Number.parseFloat(params.value[1]).toFixed(6) + ",
                                                                  "'<br/><b>", titles[3], ": </b>' + ",
                                                                  "Number.parseFloat(params.value[2]).toFixed(6))}")))) %>%
    e_mark_line(title = "Log(lambda.min)", 
                data = list(xAxis = x1, 
                            tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                              "return('<b>Log(lambda.min): </b>' + ",
                                                                              "Number.parseFloat(params.value).toFixed(6))}"))))) %>%
    e_mark_line(title = "Log(lambda.1se)", 
                data = list(xAxis = x2,
                            tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                              "return('<b>Log(lambda.1se): </b>' + ",
                                                                              "Number.parseFloat(params.value).toFixed(6))}")))))
  
  #Si se eligió manualmente un lambda
  if(!is.null(log.lambda)){
    grafico <- grafico %>% 
      e_mark_line(title = titles[4], 
                  data = list(xAxis = log.lambda,
                              lineStyle = list(color = "blue"),
                              tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                                "return('<b>Log(lambda) ",titles[4], ": </b>' + ",
                                                                                "Number.parseFloat(params.value).toFixed(6))}")))))
  }
  
  # number of non-zero coefficients at each lambda
  grafico <- grafico %>%
    e_line(nzero, x_index = 1, y_index = 1, tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                                              "return('<b>Log(lambda): </b>' + ",
                                                                                              "Number.parseFloat(params.value[0]).toFixed(6) + ",
                                                                                              "'<br/><b>", titles[5],": </b>' + ",
                                                                                              "params.value[1])}")))) %>%
    e_grid(height = "40%") %>%
    e_grid(height = "30%", top = "65%") %>%
    e_x_axis(type = 'value', minInterval = 1, min = floor(min(data.lambda$x)), gridIndex = 0, index = 0, name = "Log(lambda)") %>%
    e_y_axis(type = 'value', axisLine = list(onZero = F), gridIndex = 0, index = 0, name = titles[1]) %>%
    e_x_axis(type = 'value', minInterval = 1, min = floor(min(data.lambda$x)), gridIndex = 1, index = 1, name = "Log(lambda)") %>%
    e_y_axis(type = 'value', axisLine = list(onZero = F), gridIndex = 1, index = 1, axisLine = list(onZero = F), name = titles[5]) %>%
    e_legend(FALSE) %>% 
    e_tooltip(trigger = "item") %>% e_datazoom(show = F) %>% e_show_loading()
  
  
  return(grafico)
}



#' e_coeff_landa
#' 
#' @description Graph the coefficients and lambdas of a cv.glmnet model
#'
#' @param cv.glm a cv.glmnet model.
#' @param log.lambda number that specifies the logarithm of the selected lambda
#' @param titles labels on the chart
#'
#' @seealso \code{\link[glmnet]{cv.glmnet}}
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' 
#' @export
#' 
e_coeff_landa <- function(cv.glm, log.lambda = NULL, titles = c("Coeficientes","Seleccionado")){
  
  data   <- data.frame(t(as.data.frame(as.matrix(cv.glm$glmnet.fit$beta))))
  x      <- log(cv.glm$glmnet.fit$lambda)
  data   <- cbind(x = x, data)
  data   <- data[order(data$x),]
  #lambda <- ifelse(best.lambda %in% data$x, best.lambda, log(cv.glm$lambda.min))
  new    <- data.frame()
  for (nom in colnames(data)[-1]) {
    x      <- data[["x"]]
    y      <- data[[nom]]
    nombre <- nom
    new.   <- data.frame(x = x, y = y, nombre = nombre)
    new    <- rbind(new, new.)
  }
  
  
  grafico <- new %>%
    group_by(nombre) %>%
    e_charts(x) %>%
    e_line(y, bind = nombre, 
           tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                             "return(params.marker + ",
                                                             "'<b>' + params.seriesName + '</b><br/>' + ",
                                                             "'<b>Log(lambda): </b>' + ",
                                                             "Number.parseFloat(params.value[0].toFixed(6)) + '<br/>' + ",
                                                             "'<b>", titles[1], ": </b>' + ",
                                                             "Number.parseFloat(params.value[1].toFixed(6)))}")))) %>%
    e_mark_line(title = "Log(lambda.min)", 
                data = list(xAxis = log(cv.glm$lambda.min), lineStyle = list(color = 'black'), 
                            tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                              "return('<b>Log(lambda.min): </b>' + ",
                                                                              "Number.parseFloat(params.value).toFixed(6))}"))))) %>%
    e_x_axis(name = "Log(lambda)", axisLine = list(onZero = F)) %>%
    e_y_axis(name = titles[1],axisLine = list(onZero = F)) %>%
    e_labels(position = 'left',formatter = htmlwidgets::JS("
                                        function(params){
                                        if(params.dataIndex==0){
                                        return(params.name)
                                        }else
                                        {return('')}}"))%>%
    e_legend(show = FALSE) %>% e_tooltip() %>% 
    e_datazoom(show = F) %>% e_show_loading()
  
  #Si se eligió manualmente un lambda
  if(!is.null(log.lambda)){
    grafico <- grafico %>% 
      e_mark_line(title = titles[2], 
                  data = list(xAxis = log.lambda,
                              lineStyle = list(color = "blue"),
                              tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                                "return('<b>Log(lambda) ", titles[2],": </b>' + ",
                                                                                "Number.parseFloat(params.value).toFixed(6))}")))))
  }
  
  return(grafico)
}



#------------------------------------CODE---------------------------------------
codeRlr <- function(variable.predecir, alpha, standardize){
  return(paste0("rlr_model(data, '",variable.predecir,"', alpha = ",alpha, ", standardize = ",standardize,")"))
}

codeRlrCoeff <- function(variable.predecir, nombreModelo, log.lambda){
  return(paste0("coef_lambda(data, '", variable.predecir,"', model = ",nombreModelo,", ",
                "log.lambda = ",log.lambda, ")"))
}


codeRlrPred <- function(variable.predecir, nombreModelo, log.lambda){
  return(paste0("rlr_prediction(learning.data, test.data, '", variable.predecir,"', model = ",nombreModelo,", ",
                "log.lambda = ",log.lambda, ")"))
}


codeRlrIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.rlr)"))
}