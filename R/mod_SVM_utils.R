# # SVM PAGE ----------------------------------------------------------------------------------------------------------------
# 
# svm_model <- function(data, variable.pred, scale = TRUE, kernel = "linear"){
#   if(!is.null(variable.pred) && !is.null(data)){
#     form       <- formula(paste0(variable.pred,"~."))
#     modelo.svm <- svm(form, data, scale = scale, kernel = kernel)
#     #Cambiamos la forma en que va aparecer el call
#     modelo.svm$call$formula <- paste0(variable.pred,"~.")
#     modelo.svm$call$kernel  <- kernel
#     modelo.svm$call$scale   <- scale
#     return(modelo.svm)
#   }
#   return(NULL)
# }
# 
# svm_prediction <- function(model, test.data){
#   if(!is.null(test.data) && !is.null(model)){
#     return(predict(model,test.data))
#   }
#   return(NULL)
# }
# 
# 

#------------------------------------CODE---------------------------------------
codeSvm <- function(variable.predecir, scale, kernel){
  return(paste0("svm_model(data, '",variable.predecir,"', scale = ",scale, ", kernel = '",kernel,"')\n"))
}

codeSvmPred <- function(nombreModelo = "svm.model"){
  return(paste0("svm_prediction(model = ", nombreModelo, ", test.data)\n"))
}

codeSvmIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.svm)\n"))
}