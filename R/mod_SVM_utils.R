# # SVM PAGE ----------------------------------------------------------------------------------------------------------------
#------------------------------------CODE---------------------------------------
codeSvm <- function(variable.predecir, scale, kernel){
  return(paste0("modelo.svm <- train.svm(",variable.predecir, "~.,data = datos.aprendizaje, scale = ",scale, ", kernel = '",kernel,"')\n"))
}

codeSvmPred <- function(nombreModelo = "modelo.svm"){
  return(paste0("prediccion.svm <- predict(", nombreModelo, ", datos.prueba)\n"))
}

codeSvmIG <- function(variable.predecir){
  return(paste0("general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.svm)\n"))
}