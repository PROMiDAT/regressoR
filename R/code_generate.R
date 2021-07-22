# KNN PAGE ----------------------------------------------------------------------------------------------------------------

#' kkn_model
#' 
#' @description generates the code to create the k nearest neighbors model.
#' 
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param scale the scale parameter of the model.
#' @param kmax the kmax parameter of the model.
#' @param kernel the kernel parameter of the model.
#' @param model.var the name of the variable that stores the resulting model.
#' @param distance the distance parameter of the model.
#' 
#' @seealso \code{\link[kknn]{train.kknn}}
#' 
#' @export
#'
#' @examples
#' library(kknn)
#' x <- kkn_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.knn)
#' 
kkn_model <- function(data = "datos.aprendizaje", variable.pred = NULL, scale = TRUE, kmax = 7, kernel = "optimal", model.var = "modelo.knn", distance = 2){
  kmax <- ifelse(!is.numeric(kmax), exe("round(sqrt(nrow(",data,"))"), kmax)
  return(paste0(model.var," <- train.kknn(`",variable.pred,"`~., data = ",data,", scale =",scale,", kmax=",kmax,", kernel = '",kernel,"', distance = ",distance,")"))
}

#' kkn_prediction
#'
#' @description generates the code to create the prediction of the k nearest neighbors model.
#'
#' @param data the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#'
#'
#' @export
#'
#' @examples
#' library(kknn)
#' library(dplyr)
#' 
#' x <- kkn_model('iris', 'Petal.Length', model.var = 'model_knn')
#' exe(x)
#' print(model_knn)
#' 
#' x <- kkn_prediction('iris', 'Petal.Length', 'model_knn', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
kkn_prediction <- function(data = "datos.prueba", variable.pred = NULL, model.var = "modelo.knn", pred.var = "prediccion.knn") {
  return(paste0(pred.var," <- predict(",model.var,", ",data," %>% select(-`",variable.pred,"`))"))
}



# RD PAGE -----------------------------------------------------------------------------------------------------------------

#' rd_model
#'
#' @description generates the code to create the dimension reduction model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param n.comp the name of the variable that stores the optimum number of components.
#' @param mode the method of dimension reduction is defined as mode=1 is the MCP, and mode=0 the ACP.
#' @param scale the scale parameter of the model.
#'
#' @seealso \code{\link[pls]{pcr}}, \code{\link[pls]{plsr}}
#'
#' @export
#'
#' @examples
#' library(pls)
#' 
#' x <- rd_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.rd)
#' 
rd_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.rd",
                      n.comp = "n.comp.rd", mode = options_regressor("rd.mode"), scale = TRUE){
  mode <- ifelse(is.null(unlist(mode)), 0, mode)
  if(mode == 0){
    x <- paste0(model.var," <- pcr(`",variable.pred,"`~.,data = ",data,", scale = ",scale,", validation = 'CV')")
  }else{
    x <- paste0(model.var," <- plsr(`",variable.pred,"`~.,data = ",data,", scale = ",scale,", validation = 'CV')")
  }
  paste0(x,"\n",n.comp, " <- which.min(RMSEP(",model.var,")$val[1, 1, ]) - 1")
}

#' rd_prediction
#' 
#' @description generates the code to create the prediction of the dimension reduction model.
#'
#' @param data the name of the test data.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#' @param n.comp the name of the variable that stores the optimum number of components.
#' @param ncomp a numerical value in case you don't want to use the optimum number of components.
#'
#' @export
#'
#' @examples
#' library(pls)
#' 
#' x <- rd_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.rd)
#' 
#' x <- rd_prediction('iris', 'modelo.rd', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
rd_prediction <- function(data = "datos.prueba", model.var = "modelo.svm", pred.var = "prediccion.rd", 
                          n.comp = "n.comp.rd", ncomp = NULL) {
  ncomp <- ifelse(is.null(ncomp), n.comp, ncomp)
  paste0(pred.var," <- predict(",model.var,", ",data,", ncomp = ",ncomp,")")
}

#' rd_type
#' 
#' @description returns the name of the method of dimension reduction.
#'
#' @param mode.rd the method of dimension reduction is defined as mode=1 is the MCP, and mode=0 the ACP.
#' 
#' @seealso \code{\link[pls]{pcr}}, \code{\link[pls]{plsr}}
#'
#' @export
#'
#' @examples
#' rd_type(1)
#' rd_type(0)
#' 
rd_type <- function(mode.rd = options_regressor("rd.mode")){
  mode.rd <- ifelse(is.null(unlist(mode.rd)), 0, mode.rd)
  ifelse(mode.rd == 0, "ACP", "MCP")
}


# RF PAGE ------------------------------------------------------------------------------------------------------------

#' rf_model
#' 
#' @description generates the code to create the random forest model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param ntree the ntree parameter of the model.
#' @param mtry the mtry parameter of the model.
#'
#' @seealso \code{\link[randomForest]{randomForest}}
#'
#' @export
#'
#' @examples
#' library(randomForest)
#' x <- rf_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.rf)
#' 
rf_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.rf", ntree = 500, mtry = 1){
  ntree <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo <- paste0(model.var," <- randomForest(`",variable.pred,"`~., data = ",data,",importance = TRUE,",
                   " ntree =",ntree,",mtry =",mtry,")")
  return(codigo)
}

#' rf_prediction
#' 
#' @description generates the code to create the prediction of the random forest model.
#'
#' @param data the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#'
#' @export
#'
#' @examples
#' library(randomForest)
#' library(dplyr)
#' 
#' x <- rf_model('iris', 'Petal.Length', model.var = 'model_rf')
#' exe(x)
#' print(model_rf)
#' 
#' x <- rf_prediction('iris', 'Petal.Length', 'model_rf', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#'
rf_prediction <- function(data = "datos.prueba", variable.pred = NULL, model.var = "modelo.rf", pred.var = "prediccion.rf"){
  return(paste0(pred.var," <- predict(",model.var,", ",data," %>% select(-`",variable.pred,"`))"))
}



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
#' @param data the name of the current data.
#'
#' @export
#'
#' @examples
#' disp_models("prediction.knn", "KNN", "Species")
#' 
disp_models <- function(prediction, model_name, var_pred, data = "datos.prueba"){
  
  paste0("plot_real_prediction(",data,"[,'",var_pred,"'], ", prediction,", '",model_name,"')")
}