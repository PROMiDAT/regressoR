#' nn_model
#'
nn_model <- function(data, variable.pred, hidden = c(2), threshold = 0.1, stepmax = 2000){
  
  if(!is.null(variable.pred) && !is.null(data)){
    form <- formula(paste0(variable.pred,"~."))
    modelo.nn <- train.neuralnet(form, data = data, hidden = hidden, 
                          linear.output = TRUE, threshold = threshold, stepmax = stepmax)

    #Cambiamos la forma en que va aparecer el call
    modelo.nn$call$formula <- form
    modelo.nn$call$hidden <- hidden
    modelo.nn$call$threshold <- threshold
    modelo.nn$call$stepmax <- stepmax

    return(modelo.nn)
  }
  return(NULL)
}

#' nn_prediction
#' 
#' @description generates the prediction of a neural network model.
#'
#' @param model neural network model(neuralnet).
#' @param test.data dataframe.
#'
#' @seealso \code{\link[neuralnet]{compute}}
#'
#' @export
#'
nn_prediction <- function(model, test.data) {
  if(!is.null(test.data) && !is.null(model)){

    prediction.nn <- predict(model, test.data)$prediction
    #Como se escalaron los datos debemos volver a valor real.
   # prediction.nn <- prediction.nn * model$sd.vars[[model$variable.pred]] + model$mean.vars[[model$variable.pred]]
    return(prediction.nn)
  }
  
  return(NULL)
  # paste0("datos.dummies.prueb <- as.data.frame(scale(dummy.data.frame(",data," %>% select(-`",variable.pred,"`))))\n",
  #        "datos.dummies.prueb['",variable.pred,"'] <- NULL\n",
  #        pred.var," <- neuralnet::compute(",model.var,", datos.dummies.prueb)$net.result\n",
  #        pred.var, " <- ",pred.var," * ",sd.var,"['",variable.pred,"'] + ",mean.var,"['",variable.pred,"']")
}

#' nn_plot
#'
#' @description graph of the neural network.
#'
#' @param model a neural network model(neuralnet)
#'
#' @export
#'
nn_plot <- function(model){
  plot(model, arrow.length = 0.1, rep = 'best', intercept = TRUE,x.entry = 0.1, x.out = 0.9,
       information=FALSE,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',
       col.out='green',col.out.synapse='green', dimension=15, radius = 0.2, fontsize = 10)
  
  # paste0("plot(",model.var,", arrow.length = 0.1, rep = 'best', intercept = TRUE,x.entry = 0.1, x.out = 0.9,\n\t",
  #        "information=FALSE,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
  #        "dimension=15, radius = 0.2, fontsize = 10)")
}


#------------------------------------CODE---------------------------------------
codeNn <- function(variable.predecir, hidden, threshold, stepmax){
  return(paste0("nn_model(data, '",variable.predecir,"', hidden = ",as_string_c(hidden,quote = FALSE), ", threshold = ", threshold, ", stepmax = ",stepmax, ")\n"))
}

codeNnPred <- function(nombreModelo = "nn.model"){
  return(paste0("nn_prediction(model = ", nombreModelo, ", test.data)\n"))
}

codeNnIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.nn)\n"))
}