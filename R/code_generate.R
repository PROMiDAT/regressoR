# NN PAGE ------------------------------------------------------------------------------------------------------------

#' nn_model
#'
#' @description generates the code to create the neural network model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param mean.var the name of the variable that stores the mean of the columns.
#' @param sd.var the name of the variable that stores the standard deviation of the columns.
#' @param threshold the threshold parameter of the model.
#' @param stepmax the stepmax parameter of the model.
#' @param cant.hidden the quantity of hidden layers that are going to be used.
#' @param ... a vector with the number of nodes in each hidden layer.
#'
#' @seealso \code{\link[neuralnet]{neuralnet}}
#'
#' @export
#'
nn_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.nn", mean.var = "mean.nn",
                     sd.var = "sd.nn", threshold = 0.01, stepmax = 1000, cant.hidden = 2, ...){
  
  #Falta terminar
  
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax <- ifelse(stepmax < 100, 100, stepmax)
  capas <- as_string_c(as.numeric(list(...)[1:cant.hidden]), quote = FALSE)
  
  datos.dummies.apren <- dummy.data.frame(data)
  mean.var <- sapply(datos.dummies.apren, mean)
  sd.var <- sapply(datos.dummies.apren, sd)
  datos.dummies.apren <- as.data.frame(scale(datos.dummies.apren, center = mean.var, scale = sd.var))
  nombres <- colnames(datos.dummies.apren)
  formula.nn <- as.formula(paste(variable.pred,"~", paste0(nombres[!nombres %in% variable.pred], collapse = '+')))
  model.var <- neuralnet(formula.nn, data = datos.dummies.apren, hidden = capas, 
                         linear.output = TRUE, threshold = threshold, stepmax = stepmax)
  
  return(model.var)
  
  # paste0("datos.dummies.apren <- dummy.data.frame(",data,")\n",
  #        mean.var," <- sapply(datos.dummies.apren, mean)\n",
  #        sd.var," <- sapply(datos.dummies.apren, sd)\n",
  #        "datos.dummies.apren <- as.data.frame(scale(datos.dummies.apren, center = ",mean.var,", scale = ",sd.var,"))\n",
  #        "nombres <- colnames(datos.dummies.apren)\n",
  #        "formula.nn <- as.formula(paste('",variable.pred,"~', paste0(nombres[!nombres %in% '",variable.pred,"'], collapse = '+')))\n",
  #        model.var," <- neuralnet(formula.nn, data = datos.dummies.apren, hidden = ",capas,",\n\t\t\tlinear.output = TRUE,",
  #        "threshold = ",threshold,", stepmax = ",stepmax,")\n")
}

#' nn_prediction
#' 
#' @description generates the code to create the prediction of the neural network model.
#'
#' @param data the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#' @param mean.var the name of the variable that stores the mean of the columns.
#' @param sd.var the name of the variable that stores the standard deviation of the columns.
#'
#' @seealso \code{\link[neuralnet]{compute}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(neuralnet)
#' library(dummies)
#' library(dplyr)
#' 
#' x <- nn_model('iris', 'Petal.Length','modelo.nn', 'mean.nn', 'sd.nn', 0.05, 2000, 3, 30, 50, 30)
#' exe(x)
#' 
#' x <- nn_prediction('iris', 'Petal.Length')
#' exe(x)
#' print(prediccion.nn)
#' }
nn_prediction <- function(data = "datos.prueba", variable.pred = NULL, model.var = "modelo.nn", pred.var = "prediccion.nn", mean.var = "mean.nn", sd.var = "sd.nn") {
  paste0("datos.dummies.prueb <- as.data.frame(scale(dummy.data.frame(",data," %>% select(-`",variable.pred,"`))))\n",
         "datos.dummies.prueb['",variable.pred,"'] <- NULL\n",
         pred.var," <- neuralnet::compute(",model.var,", datos.dummies.prueb)$net.result\n",
         pred.var, " <- ",pred.var," * ",sd.var,"['",variable.pred,"'] + ",mean.var,"['",variable.pred,"']")
}

#' nn_plot
#'
#' @description generates the code to create the graph of the neural network.
#'
#' @param model.var the name of the variable that stores the resulting model.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(neuralnet)
#' library(dummies)
#' library(dplyr)
#' 
#' x <- nn_model('iris', 'Petal.Length','modelo.nn', 'mean.nn', 'sd.nn', 0.05, 2000, 3, 10, 10, 10)
#' exe(x)
#' 
#' x <- nn_plot('modelo.nn')
#' exe(x)
#' }
nn_plot <- function(model.var = "modelo.nn"){
  paste0("plot(",model.var,", arrow.length = 0.1, rep = 'best', intercept = TRUE,x.entry = 0.1, x.out = 0.9,\n\t",
         "information=FALSE,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
         "dimension=15, radius = 0.2, fontsize = 10)")
}

# MODELS UTILITY FUNCTIONS ------------------------------------------------------------------------------------------------

#' disp_models
#' 
#' @description this function generates the call code of the scatter function.
#'
#' @param prediction the name of the prediction object.
#' @param model_name the name of the model.
#' @param var_pred the name of the variable to be predicted.
#'
#' @export
#'
#' @examples
#' disp_models("prediction.knn", "KNN", "Species")
#' 
disp_models <- function(prediction, model_name, var_pred){
  
  paste0("plot_real_prediction(test.data['",var_pred,"'], ", prediction,", '",model_name,"')")
}