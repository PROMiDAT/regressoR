
# Wrapper of regressoR::exe to set the environment
exe <- function(...){
  regressoR::exe(..., envir = parent.frame())
}

# Wrapper of regressoR::models_mode to set the list of values and the language
models_mode <- function(){
  regressoR::models_mode(IndicesM)
}

# Wrapper of regressoR::comparative_table to set the list of values and the language
comparative_table <- function(sel){
  regressoR::comparative_table(sel, IndicesM)
}

#
validate_pn_data <- function(){
  regressoR::validate_pn_data(datos.originales.completos, datos.prueba.completos, variable.predecir.pn)
}

new_col <- function(){
  regressoR::new_col(datos.prueba.completos, variable.predecir.pn, predic.nuevos)
}

new_report <- function(){
  regressoR::new_report(datos.originales, input$file1$name)
}

# Pagina de RLR -------------------------------------------------------------------------------------------------------------

#Crea el modelo RL
rlr.modelo <- function(variable.pr = NULL, alpha = 0, escalar = TRUE){
  return(paste0("x <- model.matrix(",variable.pr,"~., datos.aprendizaje)[, -1]\n",
                "y <- datos.aprendizaje[, '",variable.pr,"']\n",
                "modelo.rlr.",rlr_type()," <<- glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,")"))
}

rlr.modelo.np <- function(alpha = 0, escalar = TRUE, manual = FALSE, landa = 2){
  landa <- ifelse(manual,"",paste0("cv.glm.nuevos <<- cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,")\n"))
  return(paste0("x <- model.matrix(",variable.predecir.pn,"~., datos.aprendizaje.completos)[, -1]\n",
                "y <- datos.aprendizaje.completos[, '",variable.predecir.pn,"']\n",
                landa,
                "modelo.nuevos <<- glmnet(x, y,standardize = ",escalar,", alpha = ",alpha,")"))
}

select.landa <- function(variable.pr = NULL, alpha = 0, escalar = TRUE){
  paste0("x <- model.matrix(",variable.pr,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.pr,"']\n",
         "cv.glm.",rlr_type()," <<- cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,")")
}

coeff.landas <- function(landa = NULL){
  landa <- ifelse(is.null(landa),paste0("cv.glm.",rlr_type(),"$lambda.min"), landa)
  paste0("x <- model.matrix(",variable.predecir,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.predecir,"']\n",
         "predict(modelo.rlr.",rlr_type(),", s = ",landa,", type = 'coefficients', exact = TRUE, x = x, y = y)")
}

plot.coeff.landa <- function(landa = NULL){
  landa <- ifelse(is.null(landa),paste0("cv.glm.",rlr_type(),"$lambda.min"), landa)
  paste0("plot(modelo.rlr.",rlr_type(),", 'lambda', label = TRUE)\n",
         "abline(v = log(",landa,"), col = 'blue', lwd = 2, lty = 3)")
}

#Codigo de la prediccion de rlr
rlr.prediccion <- function(landa = NULL) {
  landa <- ifelse(is.null(landa),paste0("cv.glm.",rlr_type(),"$lambda.min"), landa)
  paste0("x <- model.matrix(",variable.predecir,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.predecir,"']\n",
         "prueba <- model.matrix(",variable.predecir,"~., datos.prueba)[, -1]\n",
         "prediccion.rlr.",rlr_type()," <<- predict(modelo.rlr.",rlr_type(),",newx = prueba,",
         "s = ",landa,", exact = TRUE, x = x, y = y)")
}

rlr.prediccion.np <- function(alpha = 0, escalar = TRUE, manual = FALSE, landa = 2) {
  landa <- ifelse(manual, landa, "cv.glm.nuevos$lambda.min")
  paste0("x <- model.matrix(",variable.predecir.pn,"~., datos.aprendizaje.completos)[, -1]\n",
         "y <- datos.aprendizaje.completos[, '",variable.predecir.pn,"']\n",
         "dp <- datos.prueba.completos\n",
         "dp[, '",variable.predecir.pn,"'] <- 0\n",
         "prueba <- model.matrix(",variable.predecir.pn,"~., dp)[, -1]\n",
         "predic.nuevos <<- predict(modelo.nuevos, newx = prueba,",
         "s = ",landa,", exact = TRUE, x = x, y = y)")
}

#Codigo de la dispersion de knn
rlr.disp <- function(){
  return(disp_models(paste0("prediccion.rlr.",rlr_type()), translate("rlr"), variable.predecir))
}


# Pagina de NN ------------------------------------------------------------------------------------------------------------

#Crea el modelo NN
nn.modelo <- function(threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax <- ifelse(stepmax < 100, 100, stepmax)
  capas <- as_string_c(as.numeric(list(...)[1:cant.cap]), quote = FALSE)

  paste0("datos.dummies.apren <- dummy.data.frame(datos.aprendizaje)\n",
         "mean.nn <<- sapply(datos.dummies.apren, mean)\n",
         "sd.nn <<- sapply(datos.dummies.apren, sd)\n",
         "datos.dummies.apren <- as.data.frame(scale(datos.dummies.apren, center = mean.nn, scale = sd.nn))\n",
         "nombres <- colnames(datos.dummies.apren)\n",
         "formula.nn <- as.formula(paste('",variable.predecir,"~', paste0(nombres[!nombres %in% '",variable.predecir,"'], collapse = '+')))\n",
         "modelo.nn <<- neuralnet(formula.nn, data = datos.dummies.apren, hidden = ",capas,",\n\t\t\tlinear.output = TRUE,",
         "threshold = ",threshold,", stepmax = ",stepmax,")\n")
}

nn.modelo.np <- function(variable.pr = "",threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax <- ifelse(stepmax < 100, 100, stepmax)
  capas <- as_string_c(as.numeric(list(...)[1:cant.cap]), quote = FALSE)
  
  paste0("datos.dummies.apren <- dummy.data.frame(datos.aprendizaje.completos)\n",
         "mean.nn.np <<- sapply(datos.dummies.apren, mean)\n",
         "sd.nn.np <<- sapply(datos.dummies.apren, sd)\n",
         "datos.dummies.apren <- as.data.frame(scale(datos.dummies.apren, center = mean.nn.np, scale = sd.nn.np))\n",
         "nombres <- colnames(datos.dummies.apren)\n",
         "formula.nn <- as.formula(paste('",variable.pr,"~', paste0(nombres[!nombres %in% '",variable.pr,"'], collapse = '+')))\n",
         "modelo.nuevos <<- neuralnet(formula.nn, data = datos.dummies.apren, hidden = ",capas,",\n\t\t\tlinear.output = TRUE,",
         "threshold = ",threshold,", stepmax = ",stepmax,")\n")
}

#Codigo de la prediccion de xgb
nn.prediccion <- function() {
  selector <- -which(colnames(datos.prueba) == variable.predecir)

  paste0("datos.dummies.prueb <- as.data.frame(scale(dummy.data.frame(datos.prueba[,",selector,"])))\n",
         "datos.dummies.prueb['",variable.predecir,"'] <- NULL\n",
         "prediccion.nn <<- neuralnet::compute(modelo.nn, datos.dummies.prueb)$net.result\n",
         "prediccion.nn <<- prediccion.nn * sd.nn['",variable.predecir,"'] + mean.nn['",variable.predecir,"']")
}

nn.prediccion.np <- function(){
  selector <- -which(colnames(datos.prueba.completos) == variable.predecir.pn)

  paste0("datos.dummies.prueb <- as.data.frame(scale(dummy.data.frame(datos.prueba.completos[,",selector,"])))\n",
         "datos.dummies.prueb['",variable.predecir.pn,"'] <- NULL\n",
         "predic.nuevos <<- neuralnet::compute(modelo.nuevos, datos.dummies.prueb)$net.result\n",
         "predic.nuevos <<- predic.nuevos * sd.nn.np['",variable.predecir.pn,"'] + mean.nn.np['",variable.predecir.pn,"']\n")
}

#Codigo de la dispersion de nn
nn.disp <- function(){
  return(disp_models("prediccion.nn", translate("nn"), variable.predecir))
}

nn.plot <- function(){
  paste0("plot(modelo.nn,,arrow.length = 0.1, rep = 'best', intercept = T,x.entry = 0.1, x.out = 0.9,\n\t",
         "information=F,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
         "dimension=15, radius = 0.2, fontsize = 10)")
}

# Pagina de REPORTE ---------------------------------------------------------------------------------------------------------

recover_cat <- function(){
  unlockBinding("cat", .BaseNamespaceEnv)
  
  .BaseNamespaceEnv$cat <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE){
    if (is.character(file))
      if (file == "")
        file <- stdout()
      else if (substring(file, 1L, 1L) == "|") {
        file <- pipe(substring(file, 2L), "w")
        on.exit(close(file))
      }
      else {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
      }
      .Internal(cat(list(...), file, sep, fill, labels, append))
  }
  
  lockBinding("cat", .BaseNamespaceEnv)
}

overwrite_cat <- function(){
  unlockBinding("cat", .BaseNamespaceEnv)
  
  .BaseNamespaceEnv$cat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE){
    file <- stderr()
    sep <- ""
    
    msg <- .makeMessage(..., domain = NULL, appendLF = TRUE)
    call <- sys.call()
    cond <- simpleMessage(msg, call)
    
    if (is.character(file))
      if (file == "")
        file <- stdout()
    else if (substring(file, 1L, 1L) == "|") {
      file <- pipe(substring(file, 2L), "w")
      on.exit(close(file))
    }
    else {
      file <- file(file, ifelse(append, "a", "w"))
      on.exit(close(file))
    }
    defaultHandler <- function(c) {
      .Internal(cat(as.list(conditionMessage(c)), file, sep, fill, labels, append))
    }
    withRestarts({
      signalCondition(cond)
      defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
  }
  
  lockBinding("cat",.BaseNamespaceEnv)
}


# VARIABLES GLOBALES --------------------------------------------------------------------------------------------------------

options(language  = "es")
options(rlr.alpha = 0)

# ------------------- Datos

datos             <<- NULL
datos.originales  <<- NULL
datos.prueba      <<- NULL
datos.aprendizaje <<- NULL
variable.predecir <<- NULL
real.val          <<- NULL
contador          <<- 0
semilla           <<- FALSE
nombres.modelos   <<- c()

# ------------------- Estadisticas Basicas

correlacion   <<- NULL
cod.poder.cat <<- NULL
cod.poder.num <<- NULL

# ------------------- Modelos

IndicesM  <<- list()

# ------------------- RL

cod.rl.modelo <<- NULL
cod.rl.pred   <<- NULL
cod.rl.ind    <<- NULL

# ------------------- RLR

cod.rlr.modelo   <<- NULL
cod.rlr.pred     <<- NULL
cod.rlr.ind      <<- NULL
cod.select.landa <<- NULL

# ------------------- KNN

cod.knn.modelo <<- NULL
cod.knn.pred   <<- NULL
cod.knn.ind    <<- NULL
knn.stop.excu  <<- FALSE

# ------------------- SVM

cod.svm.modelo <<- NULL
cod.svm.pred   <<- NULL
cod.svm.ind    <<- NULL

# ------------------- DT

cod.dt.modelo <<- NULL
cod.dt.pred   <<- NULL
cod.dt.ind    <<- NULL

# ------------------- RF

cod.rf.modelo <<- NULL
cod.rf.pred   <<- NULL
cod.rf.ind    <<- NULL
rf.stop.excu  <<- FALSE

# ------------------- BOOSTING

cod.b.modelo <<- NULL
cod.b.pred   <<- NULL
cod.b.ind    <<- NULL

# ------------------- NN

cod.nn.modelo <<- NULL
cod.nn.pred   <<- NULL
cod.nn.ind    <<- NULL
NN_EXECUTION  <<- TRUE
mean.nn       <<- NULL
sd.nn         <<- NULL
mean.nn.np    <<- NULL
sd.nn.np      <<- NULL

# ------------------- Prediccion Nuevos

datos.originales.completos  <<- NULL
datos.aprendizaje.completos <<- NULL
datos.prueba.completos      <<- NULL
variable.predecir.pn        <<- NULL
modelo.seleccionado.pn      <<- NULL
contadorPN                  <<- 0
code.trans.pn               <<- ""
modelo.nuevos               <<- NULL
predic.nuevos               <<- NULL

