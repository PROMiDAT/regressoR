codigo.modelo <- function(model.name = "knn", variable.pr = NULL, datos = "datos.aprendizaje"){
  return(paste0("modelo.",model.name," <<- train.",model.name,"(",variable.pr,"~., data = ",datos,")\n"))
}

codigo.prediccion <- function(model.name = "knn", alg = NULL, datos = "datos.prueba"){
  if (is.null(alg)) {
    return(paste0("prediccion.",model.name," <<- predict(modelo.",model.name,", ",datos,")\n"))
  }else{
    return(paste0("prediccion.",model.name,".",alg," <<- predict(modelo.",model.name,".",alg,", ",datos,")\n"))
  }
}

#Codigo de la matriz de confucion de Bayes
codigo.IG <- function(model.name = "knn", alg = NULL, datos = "datos.prueba", variable.pr = NULL){
  if (is.null(alg)) {
    return(paste0("general_indices(",datos,"[,'",variable.pr,"'], prediccion.",model.name,")\n"))
    }
  else{
    return(paste0("general_indices(",datos,"[,'",variable.pr,"'], prediccion.",model.name,".",alg,")\n"))
  }  

}  
