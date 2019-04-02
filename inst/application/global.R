

# Wrapper of regressoR::error.variables to set the language
error.variables <- function(num = T){
  regressoR::error.variables(num, input$idioma)
}

# Wrapper of regressoR::translate to set the language
translate <- function(labelid){
  regressoR::translate(labelid, input$idioma)
}

# Wrapper of regressoR::render_table_data to set the language
render_table_data <- function(data, editable = TRUE, dom = "frtip", pageLength = 10, scrollY = "27vh", server = T, language = "es"){
  regressoR::render_table_data(data, editable, dom, pageLength, scrollY, server, input$idioma)
}

# Wrapper of regressoR::exe to set the environment
exe <- function(...){
  regressoR::exe(..., envir = parent.frame())
}

# Wrapper of regressoR::models_mode to set the list of values and the language
models_mode <- function(){
  regressoR::models_mode(IndicesM, input$idioma)
}

# Wrapper of regressoR::comparative_table to set the list of values and the language
comparative_table <- function(sel){
  regressoR::comparative_table(sel, IndicesM, input$idioma)
}

#
validate_pn_data <- function(){
  regressoR::validate_pn_data(datos.originales.completos, datos.prueba.completos, variable.predecir.pn, input$idioma)
}

new_col <- function(){
  regressoR::new_col(datos.prueba.completos, variable.predecir.pn, predic.nuevos)
}

new_report <- function(){
  regressoR::new_report(datos.originales, input$file1$name)
}

#####

disp_models <- function(prediction, model_name){
  regressoR::disp_models(prediction, model_name, variable.predecir)
}

# Pagina de Correlacion -----------------------------------------------------------------------------------------------------

#Calcula la matriz de correlacion
modelo.cor <- function(data = "datos"){
  return(paste0("correlacion <<- cor(var_numerical(", data, "))"))
}

#Codigo de la generacion de correlaciones
correlaciones <- function(metodo = 'circle', tipo = "lower"){
  return(paste0("corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black',
                tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
}

# Pagina de Poder Predictivo ------------------------------------------------------------------------------------------------

#Grafica el pairs

#Genera el resumen numerico de una variable
resumen.numerico <- function(data, variable) {
  datos.numericos <- list(
    Q1 = list(
      id = "q1", Label = tags$span(`data-id`="q1", translate("q1")), color = "green",
      Value = format(round(quantile(data[, variable], .25), 3), scientific = F)
    ),
    Mediana = list(
      id = "mediana", Label = tags$span(`data-id`="mediana", translate("mediana")),
      Value = format(round(median(data[, variable]), 3), scientific = F),
      color = "orange"),
    Q3 = list(
      id = "q3", Label = tags$span(`data-id`="q3", translate("q3")), color = "maroon",
      Value = format(round(quantile(data[, variable], .75), 3), scientific = F)
    ),
    Minimo = list(
      id = "minimo", Label = tags$span(`data-id`="minimo", translate("minimo")),
      Value = format(round(min(data[, variable]), 3), scientific = F),
      color = "red"),
    Promedio = list(
      id = "promedio", Label = tags$span(`data-id`="promedio", translate("promedio")),
      Value = format(round(mean(data[, variable]), 3), scientific = F),
      color = "blue"),
    Maximo = list(
      id = "maximo", Label = tags$span(`data-id`="maximo", translate("maximo")),
      Value = format(round(max(data[, variable]), 3), scientific = F),
      color = "purple"),
    DS <- list(
      id = "ds", Label = tags$span(`data-id`="ds", translate("ds")), color = "yellow",
      Value = format(round(sd(data[, variable]), 3), scientific = FALSE, nsmall = 3)
    )
  )
  
  res <- lapply(datos.numericos, function(i) {
    tags$div(
      class='shiny-html-output col-sm-6 shiny-bound-output', id=i$id,
      tags$div(
        class=paste0('small-box bg-', i$color),
        tags$div(class='inner', tags$h3(i$Value), tags$p(i$Label)),
        tags$div(class='icon-large', tags$i(class=i$icon))
      )
    )
  })
  return(res)
}

#Genera el resumen categorico de una variable
resumen.categorico <- function(data, variable){
  color <- c("red","yellow","aqua","navy","teal","olive","purple","maroon",
             "black","blue","lime","orange","light-blue","green","fuchsia")
  datos.categoricos <- levels(data[, variable])
  res <- lapply(datos.categoricos, function(i) {
    tags$div(
      class='shiny-html-output col-sm-6 shiny-bound-output', id=paste0(variable, i),
      tags$div(
        class=paste0('small-box bg-', sample(color, 1)),
        tags$div(class='inner', tags$h3(summary(data[, variable])[i]), tags$p(i))
      )
    )
  })
  return(res)
}



#Hace el grafico de la distribucion categorica
distribucion.categorico <- function(var) {
  colores <- sapply(levels(var),function(i) rgb(runif(1), runif(1), runif(1), 0.8))
  data <- data.frame(label = levels(var), value = summary(var))
  ggplot(data, aes(label, value)) +
    geom_bar(stat = 'identity', fill = colores) +
    geom_text(aes(label = value, y = value), vjust = -0.5, size = 4) +
    theme_minimal()
}

# Pagina de RL --------------------------------------------------------------------------------------------------------------

#Crea el modelo RL
rl.modelo <- function(variable.pr = NULL){
  return(paste0("modelo.rl <<- lm(",variable.pr,"~., data = datos.aprendizaje)"))
}

rl.modelo.np <- function(){
  return(paste0("modelo.nuevos <<- lm(",variable.predecir.pn,"~., data = datos.aprendizaje.completos)"))
}

#Codigo de la prediccion de rl
rl.prediccion <- function(variable.pr = NULL) {
  return(paste0("prediccion.rl <<- predict(modelo.rl, datos.prueba)"))
}

rl.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos)"))
}

#Codigo de la dispersion de knn
rl.disp <- function(){
  return(disp_models("prediccion.rl", translate("rll")))
}

# Pagina de RLR -------------------------------------------------------------------------------------------------------------

rlr.type <- function(){
  ifelse(input$alpha.rlr == 0, "ridge", "lasso")
}

#Crea el modelo RL
rlr.modelo <- function(variable.pr = NULL, alpha = 0, escalar = TRUE){
  return(paste0("x <- model.matrix(",variable.pr,"~., datos.aprendizaje)[, -1]\n",
                "y <- datos.aprendizaje[, '",variable.pr,"']\n",
                "modelo.rlr.",rlr.type()," <<- glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,")"))
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
         "cv.glm.",rlr.type()," <<- cv.glmnet(x, y, standardize = ",escalar,", alpha = ",alpha,")")
}

coeff.landas <- function(landa = NULL){
  landa <- ifelse(is.null(landa),paste0("cv.glm.",rlr.type(),"$lambda.min"), landa)
  paste0("x <- model.matrix(",variable.predecir,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.predecir,"']\n",
         "predict(modelo.rlr.",rlr.type(),", s = ",landa,", type = 'coefficients', exact = TRUE, x = x, y = y)")
}

plot.coeff.landa <- function(landa = NULL){
  landa <- ifelse(is.null(landa),paste0("cv.glm.",rlr.type(),"$lambda.min"), landa)
  paste0("plot(modelo.rlr.",rlr.type(),", 'lambda', label = TRUE)\n",
         "abline(v = log(",landa,"), col = 'blue', lwd = 2, lty = 3)")
}

#Codigo de la prediccion de rlr
rlr.prediccion <- function(landa = NULL) {
  landa <- ifelse(is.null(landa),paste0("cv.glm.",rlr.type(),"$lambda.min"), landa)
  paste0("x <- model.matrix(",variable.predecir,"~., datos.aprendizaje)[, -1]\n",
         "y <- datos.aprendizaje[, '",variable.predecir,"']\n",
         "prueba <- model.matrix(",variable.predecir,"~., datos.prueba)[, -1]\n",
         "prediccion.rlr.",rlr.type()," <<- predict(modelo.rlr.",rlr.type(),",newx = prueba,",
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
  return(disp_models(paste0("prediccion.rlr.",rlr.type()), translate("rlr")))
}

# Pagina de KNN -------------------------------------------------------------------------------------------------------------

#Crea el modelo KNN
kkn.modelo <- function(variable.pr = NULL, scale = TRUE,kmax = 7, kernel = "optimal"){
  kmax <- ifelse(!is.numeric(kmax), round(sqrt(nrow(datos.aprendizaje))), kmax)
  return(paste0("modelo.knn.",kernel," <<- train.kknn(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')"))
}

kkn.modelo.np <- function(scale = TRUE,kmax = 7, kernel = "optimal"){
  kmax <- ifelse(!is.numeric(kmax), round(sqrt(nrow(datos.aprendizaje))), kmax)
  return(paste0("modelo.nuevos <<- train.kknn(",variable.predecir.pn,"~., data = datos.aprendizaje.completos, scale =",scale,", kmax=",kmax,", kernel = '",kernel,"')"))
}

#Codigo de la prediccion de knn
kkn.prediccion <- function(kernel = "optimal") {
  return(paste0("prediccion.knn.",kernel," <<- predict(modelo.knn.",kernel,", datos.prueba[,-which(colnames(datos.prueba) == '",variable.predecir,"')])"))
}

kkn.prediccion.pn <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')])"))
}

#Codigo de la dispersion de knn
knn.disp <- function(kernel = "optimal"){
  return(disp_models(paste0("prediccion.knn.",kernel), translate("knnl")))
}

# Pagina de SVM -------------------------------------------------------------------------------------------------------------

#Crea el modelo SVM
svm.modelo <- function(variable.pr = NULL, scale = TRUE, kernel = "linear"){
  return(paste0("modelo.svm.",kernel," <<- svm(",variable.pr,"~., data = datos.aprendizaje, scale =",scale,", kernel = '",kernel,"')"))
}

svm.modelo.np <- function(scale = TRUE, kernel = "linear"){
  return(paste0("modelo.nuevos <<- svm(",variable.predecir.pn,"~., data = datos.aprendizaje.completos, scale =",scale,", kernel = '",kernel,"')"))
}

#Codigo de la prediccion de svm
svm.prediccion <- function(kernel = "linear") {
  return(paste0("prediccion.svm.",kernel," <<- predict(modelo.svm.",kernel," , datos.prueba[,-which(colnames(datos.prueba) == '",variable.predecir,"')])"))
}

svm.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos , datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')])"))
}

#Codigo de la dispersion de knn
svm.disp <- function(kernel = "linear"){
  return(disp_models(paste0("prediccion.svm.",kernel), translate("svml")))
}

# Pagina de DT --------------------------------------------------------------------------------------------------------------

#Crea el modelo DT
dt.modelo <- function(variable.pr = NULL, minsplit =  20, maxdepth = 15){
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit )
  maxdepth <- ifelse(!is.numeric(maxdepth) || maxdepth > 30, 15, maxdepth)
  codigo <- paste0("modelo.dt <<- rpart(",variable.pr,"~., data = datos.aprendizaje,
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"))")
  return(codigo)
}

dt.modelo.np <- function(variable.pr = NULL, minsplit =  20, maxdepth = 15){
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit )
  maxdepth <- ifelse(!is.numeric(maxdepth) || maxdepth > 30, 15, maxdepth)
  codigo <- paste0("modelo.nuevos <<- rpart(",variable.pr,"~., data = datos.aprendizaje.completos,
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"))")
  return(codigo)
}

#Codigo de la prediccion de DT
dt.prediccion <- function() {
  return(paste0("prediccion.dt <<- predict(modelo.dt, datos.prueba)"))
}

dt.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos)"))
}

#Codigo del grafico de dt
dt.plot <- function(){
  num <- length(levels(datos[,variable.predecir]))
  return(paste0("prp(modelo.dt, type = 2, extra = 100, nn = T, varlen = 0, faclen = 0,
fallen.leaves = TRUE, branch.lty = 6, shadow.col = '#dedede',box.col = '#c8b028')")) ##c8b028!important
}

#Codigo de la dispersion de knn
dt.disp <- function(){
  return(disp_models("prediccion.dt", translate("dtl")))
}

# Pagina de RF --------------------------------------------------------------------------------------------------------------

#Crea el modelo RF
rf.modelo <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo <- paste0("modelo.rf <<- randomForest(",variable.pr,"~., data = datos.aprendizaje,importance = TRUE,",
                   " ntree =",ntree,",mtry =",mtry,")")
  return(codigo)
}

rf.modelo.np <- function(variable.pr = NULL, ntree = 500, mtry = 1){
  ntree <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo <- paste0("modelo.nuevos <<- randomForest(",variable.pr,"~., data = datos.aprendizaje.completos,importance = TRUE,",
                   " ntree =",ntree,",mtry =",mtry,")")
  return(codigo)
}

#Codigo de la prediccion de rf
rf.prediccion <- function(variable.pr = NULL) {
  return(paste0("prediccion.rf <<- predict(modelo.rf,datos.prueba[,-which(colnames(datos.prueba) == '",variable.pr,"')])"))
}

rf.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')])"))
}

importance.plor.rf <- function(modelo.rf, titulo.1, titulo.2){
  importancia <- randomForest::importance(modelo.rf) %>% as.data.frame() %>% tibble::rownames_to_column("Variable")
  g1 <- ggplot(importancia, aes(x = fct_reorder(Variable, `%IncMSE`), y = `%IncMSE`, fill = fct_reorder(Variable, `%IncMSE`))) + 
    geom_bar(stat = 'identity', position = 'identity', width = 0.1) +
    labs(title = titulo.1,  y = "", x = "") +
    scale_y_continuous(labels = scales::comma) + coord_flip() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10),legend.position = "none")
  g2 <- ggplot(importancia, aes(x = fct_reorder(Variable, IncNodePurity), y = IncNodePurity, fill = fct_reorder(Variable, IncNodePurity))) + 
    geom_bar(stat = 'identity', position = 'identity', width = 0.1) +
    labs(title = titulo.2,  y = "", x = "") +
    scale_y_continuous(labels = scales::comma) + coord_flip() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(size = 10), legend.position = 'none')
  print(gridExtra::grid.arrange(g1, g2, ncol = 2, nrow = 1))
}

#Codigo de la dispersion de knn
rf.disp <- function(){
  return(disp_models("prediccion.rf", translate("rfl")))
}

# Pagina de BOOSTING --------------------------------------------------------------------------------------------------------

calibrar.boosting <- function(){
  nr <- nrow(datos.aprendizaje)
  for(i in 10:1){
    for (j in seq(0.5, 1, 0.1)) {
      if(nr * j > i*2 + 1){
        return(list(n.minobsinnode = i, bag.fraction = j))
      }
    }
  }
  return(NULL)
}

#Crea el modelo BOOSTING
boosting.modelo <- function(variable.pr = NULL, iter = 50, type = "gaussian", minsplit = 0.1){
  iter <- ifelse(!is.numeric(iter), 50, iter)
  minsplit <- ifelse(!is.numeric(minsplit), 0.1, minsplit)
  extra.values <- calibrar.boosting()
  if(is.null(extra.values)){
    codigo <- paste0("modelo.boosting.",type," <<- gbm(",variable.pr,
                     "~ ., data = datos.aprendizaje, distribution = '",
                     type,"', n.trees = ",iter,", shrinkage = ",minsplit,")")
  }else{
    codigo <- paste0("modelo.boosting.",type," <<- gbm(",variable.pr,
                     "~ ., data = datos.aprendizaje, distribution = '",
                     type,"', n.trees = ",iter,", shrinkage = ",minsplit,",n.minobsinnode = ",extra.values[["n.minobsinnode"]],
                     ",bag.fraction = ",extra.values[["bag.fraction"]],")")
  }
  return(codigo)
}

calibrar.boosting.np <- function(){
  nr <- nrow(datos.aprendizaje.completos)
  for(i in 10:1){
    for (j in seq(0.5, 1, 0.1)) {
      if(nr * j > i*2 + 1){
        return(list(n.minobsinnode = i, bag.fraction = j))
      }
    }
  }
  return(NULL)
}

boosting.modelo.np <- function(variable.pr = NULL, iter = 50, type = "gaussian", minsplit = 0.1){
  iter <- ifelse(!is.numeric(iter), 50, iter)
  minsplit <- ifelse(!is.numeric(minsplit), 0.1, minsplit)
  extra.values <- calibrar.boosting.np()
  if(is.null(extra.values)){
    codigo <- paste0("modelo.nuevos <<- gbm(",variable.pr,
                    "~ ., data = datos.aprendizaje.completos, distribution = '",
                    type,"', n.trees = ",iter,", shrinkage = ",minsplit,")")
  }else{
    codigo <- paste0("modelo.nuevos <<- gbm(",variable.pr,
                     "~ ., data = datos.aprendizaje.completos, distribution = '",
                     type,"', n.trees = ",iter,", shrinkage = ",minsplit,",n.minobsinnode = ",extra.values[["n.minobsinnode"]],
                     ",bag.fraction = ",extra.values[["bag.fraction"]],")")
  }
  return(codigo)
}

#Codigo de la prediccion de boosting
boosting.prediccion <- function(variable.pr = NULL, type = "gaussian") {
  return(paste0("prediccion.boosting.",type," <<- predict(modelo.boosting.",type,
                ", datos.prueba[,-which(colnames(datos.prueba) == '",variable.pr,"')],n.trees = ",input$iter.boosting,")"))
}

boosting.prediccion.np <- function() {
  return(paste0("predic.nuevos <<- predict(modelo.nuevos, datos.prueba.completos[,-which(colnames(datos.prueba.completos) == '",variable.predecir.pn,"')]",
                ",n.trees = ",input$iter.boosting.pred,")"))
}

#Codigo de la matriz de confucion de boosting
boosting.MC <- function(variable.p, type = "gaussian"){
  return(paste0("real <- datos.prueba$",variable.p,"\n",
                "prediccion <- prediccion.boosting.",type,"\n",
                "MC.boosting.",type," <<- table(real, prediccion)"))
}

#Codigo del grafico de boosting
boosting.plot.import <- function(type = "gaussian"){
  paste0("ggplot(summary(modelo.boosting.",type,"), aes(x = fct_reorder(var, rel.inf), y = rel.inf, fill = fct_reorder(var, rel.inf))) +\n",
    "geom_bar(stat = 'identity', position = 'identity', width = 0.1) +\n",
    "labs(title = '",translate("impVarRI"),"', y = '",translate("RI"),"', x = '')+\n",
    "scale_y_continuous(labels = scales::comma) + coord_flip() +\n",
    "theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')\n")
}

#Codigo de la dispersion de knn
boosting.disp <- function(type = "gaussian"){
  return(disp_models(paste0("prediccion.boosting.",type), translate("bl")))
}

# Pagina de NN ------------------------------------------------------------------------------------------------------------

#Crea el modelo NN
nn.modelo <- function(threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax <- ifelse(stepmax < 100, 100, stepmax)
  capas <- as.string.c(as.numeric(list(...)[1:cant.cap]), quote = FALSE)

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
  capas <- as.string.c(as.numeric(list(...)[1:cant.cap]), quote = FALSE)
  
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
  return(disp_models("prediccion.nn", translate("nn")))
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

