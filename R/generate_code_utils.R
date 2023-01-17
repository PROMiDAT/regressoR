codigo.modelo <- function(model.name = "knn", variable.pr = NULL, datos = "datos.aprendizaje"){
  return(paste0("modelo.",model.name," <- train.",model.name,"(",variable.pr,"~., data = ",datos,")\n"))
}

codigo.prediccion <- function(model.name = "knn", alg = NULL, datos = "datos.prueba"){
  if (is.null(alg)) {
    return(paste0("prediccion.",model.name," <- predict(modelo.",model.name,", ",datos,")\n"))
  }else{
    return(paste0("prediccion.",model.name,".",alg," <- predict(modelo.",model.name,".",alg,", ",datos,")\n"))
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

# SVM ----------------------------------------------------------------------------------------------------------------
codeSvm <- function(variable.predecir, scale, kernel, datos = "datos.aprendizaje"){
  return(paste0("modelo.svm <- train.svm(",variable.predecir, "~.,data =  ",datos,", scale = ",scale, ", kernel = '",kernel,"')\n"))
}

# KNN ----------------------------------------------------------------------------------------------------------------
codeKnn <- function(variable.predecir, scale, k, kernel, distance, datos = "datos.aprendizaje"){
  return(paste0("modelo.knn <- train.knn(",variable.predecir,"~.,data = ",datos,", scale = ",scale, ", k = ", k,
                ", kernel = '",kernel,"', distance = ", distance, ")\n"))
}

# BOOSTING ----------------------------------------------------------------------------------------------------------------
codeBoost <- function(variable.predecir, n.trees, distribution, shrinkage, datos = "datos.aprendizaje"){
  return(paste0("modelo.boosting <- boosting_model(data = ",datos,", '",variable.predecir,"', n.trees = ",n.trees, ", distribution = '", distribution, "', shrinkage = ",shrinkage, ")\n"))
}

# RD ----------------------------------------------------------------------------------------------------------------
codeRd <- function(variable.predecir, mode, scale, datos = "datos.aprendizaje"){
  return(paste0("modelo.rd <- rd_model(data = ",datos,", '",variable.predecir,"', mode = ",mode, ", scale = ", scale, ")\n"))
}

# RL ----------------------------------------------------------------------------------------------------------------
codeRl <- function(variable.predecir, datos = "datos.aprendizaje"){
  return(paste0("modelo.rl <- lm(",variable.predecir,"~.,data = ",datos,")\n"))
}

codeRlCoef <- function(nombreModelo = "modelo.rl"){
  return(paste0("information <- rl_coeff(",nombreModelo,")\n",
                "information$df.rl[,c(1,4)]\n"))
}

# NN ----------------------------------------------------------------------------------------------------------------
codeNn <- function(variable.predecir, hidden, threshold, stepmax, datos = "datos.aprendizaje"){
  return(paste0("modelo.nn <- train.neuralnet(",variable.predecir,"~., data = ",datos,", hidden = ",as_string_c(hidden,quote = FALSE), ", threshold = ", threshold, ", stepmax = ",stepmax, ")\n"))
}

# RLR ----------------------------------------------------------------------------------------------------------------
codeRlr <- function(variable.predecir, alpha, standardize, datos = "datos.aprendizaje"){
  return(paste0("modelo.rlr <- rlr_model(",datos,", '",variable.predecir,"', alpha = ",alpha, ", standardize = ",standardize,")\n"))
}

codeRlrCoeff <- function(variable.predecir, nombreModelo, log.lambda = NULL, datos = "datos.aprendizaje"){
  param.lambda <- ifelse(is.null(log.lambda),"",paste0(", log.lambda = ",log.lambda))
  return(paste0("coef_lambda(",datos,", '", variable.predecir,"', model = ",nombreModelo,
                param.lambda, ")\n"))
}

codeRlrPred <- function(nombreModelo, variable.predecir, log.lambda = NULL, datos = "datos.prueba"){
  param.lambda <- ifelse(is.null(log.lambda),"",paste0(", log.lambda = ",log.lambda))
  return(paste0("prediccion.rlr <- rlr_prediction(model = modelo.",nombreModelo, ", ",datos,", " , "'", variable.predecir,"'",param.lambda, ")\n"))
}

# RF ----------------------------------------------------------------------------------------------------------------
codeRf <- function(variable.predecir, ntree, mtry, datos = "datos.aprendizaje"){
  return(paste0("modelo.rf <- train.randomForest(",variable.predecir,"~., data = ",datos,", ntree = ",ntree, ", mtry = ", mtry, ")\n"))
}

# DT ----------------------------------------------------------------------------------------------------------------
codeDt <- function(variable.predecir, minsplit, maxdepth, datos = "datos.aprendizaje"){
  return(paste0("modelo.dt <- train.rpart(",variable.predecir,"~.,  ",datos,", minsplit = ",minsplit, ", maxdepth = ",maxdepth,")\n"))
}

codeDtPlot <- function(nombreModelo){
  return(paste0("dt_plot(modelo.dt)\n"))
}