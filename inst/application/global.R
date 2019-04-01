
# FUNCIONES GLOBALES --------------------------------------------------------------------------------------------------------

#Colores de ggplot2
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

max.col <- function(m){
  base::max.col(apply(m, 1, function(x) max(x, na.rm = TRUE)) == m)
}

#Obtiene los nombres de columnas o regresa un string vacio
colnames.empty <- function(res){
  res <- colnames(res)
  if(is.null(res))
    return("")
  return(res)
}

#Obtiene solo las variables numericas
var.numericas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#Obtiene solo las variables categoricas
var.categoricas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#Codigo del calculo de los indices
# Funciones para medir precisión
indices.generales <- function(real, prediccion) {
  RMSE <- sqrt(sum((real - prediccion) ^ 2) / length(prediccion))
  MAE  <- sum(abs(real - prediccion)) / length(prediccion)
  RE   <- sum(abs(real - prediccion)) / sum(abs(real))
  COR  <- as.numeric(cor(real, prediccion))
  COR  <- ifelse(is.na(COR), 0 , COR)
  return(list(Raiz.Error.Cuadratico = RMSE,
              Error.Absoluto = MAE,
              Error.Relativo = RE,
              Correlacion = COR))
}

indices.resumen <- function(){
  l <- list()
  l["Min"] <- min(datos.aprendizaje[,variable.predecir])
  l["1Q"] <- quantile(datos.aprendizaje[,variable.predecir], prob=c(0.25))
  l["3Q"] <- quantile(datos.aprendizaje[,variable.predecir], prob=c(0.75))
  l["Max"] <- max(datos.aprendizaje[,variable.predecir])
  return(l)
}

# Gráfico de dispersión entre el valor real de la variable a predecir y la predicción del modelo.
plot.real.prediccion <- function(real, prediccion, modelo = "") {
  ggplot(data = data.frame(Real = real, Prediccion = as.numeric(prediccion)), mapping = aes(x = Real, y = Prediccion)) +
    geom_point(size = 2, col = "red") +
    labs(title = paste0("Real vs Predicción", ifelse(modelo == "", "", paste(", con", modelo))), x = "Real", y = "Predicción") +
    theme_minimal() +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_line(col = "black",  mapping = aes(x = Real, y = Real), alpha = 0.5)
}

disp.modelos <- function(prediccion, modelo){
  paste0("plot.real.prediccion(datos.prueba[,'",variable.predecir,"'], ",prediccion,", '",modelo,"')")
}

#Convierte una tabla de prediccion html a data.frame
dt.to.data.frame.predict <- function(datos){
  datos <- datos$x$data
  return(datos)
}

# Hace el grafico de la matriz de confusion
plot.MC.code <- function(cm) {
  return(paste0("
plot.MC <<- function(cm) {
  par(mar = c(2, 2, 2, 2))
  plot(c(1, 600), c(-100, 500), type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
  title('",tr("mc"),"', cex.main = 2)

  start <- 80
  len <- 500 - start

  n.class <- ncol(cm)
  names.class <- colnames(cm)
  prec.cat <- diag(cm) / rowSums(cm)
  error.cat <- 1 - prec.cat

  ancho <- len / n.class
  alto <- len / (n.class)
  x2 <- (x1 <- start) + ancho
  y2 <- (y1 <- len) - alto

  text(310, 485, '",tr("pred"),"', cex = 1.3, font = 2)
  text(start - 55, 250, 'Real', cex = 1.3, srt = 90, font = 2)

  for (i in 0:(n.class - 1)) {
    for (j in 0:(n.class - 1)) {
      x1.aux <- x1 + j * (ancho + 3)
      y1.aux <- y1 - i * (alto + 5)
      x2.aux <- x2 + j * (ancho + 3)
      y2.aux <- y2 - i * (alto + 5)
      if (j < (n.class)) {
        rect(x1.aux, y1.aux, x2.aux, y2.aux, col = ifelse(i == j, '#3f72af', '#11999e'))
        text(mean(c(x1.aux, x2.aux)),
          mean(c(y1.aux, y2.aux)),
          paste0(cm[(i + 1), (j + 1)], ' (', round(cm[(i + 1), (j + 1)] / sum(cm[(i + 1), ]), 2) * 100, '%)'),
          cex = 1.1, font = 2, col = 'white')
      }
    }
    text(mean(c((x2 + i * (ancho + 3)), (x1 + i * (ancho + 3)))), y1 + 20, names.class[i + 1], cex = 1)
    text(x1 - 20, mean(c((y1 - i * (alto + 5)), (y2 - i * (alto + 5)))), names.class[i + 1], cex = 1)
  }
  text(mean(c((x2 + (i + 1) * (ancho + 3)), (x1 + (i + 1) * (ancho + 3)))), y1 + 20, names.class[i + 2], cex = 1.2)
  text(mean(c((x2 + (i + 2) * (ancho + 3)), (x1 + (i + 2) * (ancho + 3)))), y1 + 20, names.class[i + 3], cex = 1.2)
  text(mean(c((x2 + (i + 3) * (ancho + 3)), (x1 + (i + 3) * (ancho + 3)))), y1 + 20, names.class[i + 4], cex = 1.2)
}"))
}

# Concatena y ejecuta un string como codigo
exe <- function(...){
  eval(parse(text = paste0(...)))
}

as.string.c <- function(vect, .numeric = FALSE){
  if(.numeric){
    return(paste0("c(",paste0(vect, collapse = ","),")"))
  }
  else{
    return(paste0("c('",paste0(vect, collapse = "','"),"')"))
  }
}

extract.code <- function(funcion) {
  code <- paste(head(exe(funcion), 100), collapse = "\n")
  code <- paste(funcion, "<-", code)
  return(code)
}

# Pagina de Cargar y Transformar Datos --------------------------------------------------------------------------------------

#Transforma las variables a disyuntivas
datos.disyuntivos <- function(data, vars){
  if(is.null(data)) return(NULL)
  cualitativas <- base::subset(data, select = colnames(data) %in% c(vars))
  data <- data[, !colnames(data) %in% vars]
  for (variable in colnames(cualitativas)) {
    for (categoria in unique(cualitativas[, variable])) {
      nueva.var <- as.numeric(cualitativas[, variable] == categoria)
      data <- cbind(data, nueva.var)
      colnames(data)[length(colnames(data))] <- paste0(variable, '.', categoria)
    }
  }
  return(data)
}

#Genera el codigo para cargar datos
code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";", sep.decimal = ",", encabezado = T, d.o = "datos.originales", d = "datos" ){
  if(!is.null(ruta)){
    ruta <-  gsub("\\", "/", ruta, fixed=TRUE)
  }
  if(nombre.filas){
    return(paste0( d.o ," <<- read.table('", ruta, "', header=",
                  encabezado, ", sep='", separador, "', dec = '", sep.decimal, "', row.names = 1) \n",d," <<- ",d.o))
  } else {
    return(paste0(d.o, "<<- read.table('", ruta, "', header=", encabezado, ", sep='", separador, "', dec = '", sep.decimal,
                  "') \n",d," <<- ",d.o))
  }
}

#Eliminar NAs
code.NA <- function(deleteNA = T, d.o = "datos.originales") {
  res <- ifelse(deleteNA, paste0(d.o, "<<- na.omit(",d.o,")\n"),
                paste0("Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
                       "for (variable in colnames(",d.o,")) {\n",
                       "  if(any(is.na(",d.o,"[, variable]))){\n",
                       "   ifelse(class(",d.o,"[, variable]) %in% c('numeric', 'integer'),\n",
                       "           ",d.o,"[, variable][is.na(",d.o,"[, variable])] <<- \n",
                       "                                              mean(",d.o,"[, variable], na.rm = T),\n",
                       "           ",d.o,"[, variable][is.na(",d.o,"[, variable])] <<- \n",
                       "                                     Mode(",d.o,"[, variable]))",
                       "\n   }\n}"))
  return(res)
}

#Genera el codigo para transformar datos
code.trans <- function(variable, nuevo.tipo, d.o = "datos.originales",d="datos"){
  if(nuevo.tipo == "categorico"){
    return(paste0(d,"[, '", variable, "'] <<- as.factor(",d,"[, '", variable, "'])"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0(d,"[, '", variable, "'] <<- as.numeric(sub(',', '.', ",d,"[, '", variable, "'], fixed = TRUE))"))
  } else {
    es.factor <- ifelse( eval(parse(text = paste0("class(",d.o,"[, variable]) %in% c('numeric', 'integer')"))),
                        paste0(d,"[, '", variable, "'] <<- as.factor(",d,"[, '", variable, "']) \n"), "")
    return(paste0(es.factor, d, " <<- datos.disyuntivos(",d,", '", variable,"')"))
  }
}

#Desactiva las variables seleccionadas de los datos
code.desactivar <- function(variables, d = "datos"){
  return(paste0(d, " <<- subset(",d,", select = -c(", paste(variables, collapse = ","), "))"))
}

# Pagina de Segmentar Datos -------------------------------------------------------------------------------------------------

#Crea el código de la particion en testing y learning data
particion.code <- function(data = "datos", p = "0.5", variable = NULL, semilla = 5, perm.semilla = FALSE){
  variable.predecir <<- variable
  semilla <- ifelse(is.numeric(semilla), semilla, 5)
  codigo <- ifelse(perm.semilla, paste0("set.seed(",semilla,")"), "rm(.Random.seed, envir = globalenv())")
  codigo <- paste0(codigo,"\nvariable.predecir <<- '",variable,"'\nparticion <- sample(1:nrow(datos),size = nrow(datos)*",p/100,", replace = FALSE)\n
datos.prueba <<- datos[-particion,]\ndatos.aprendizaje <<- datos[particion,]")
  codigo <- ifelse(perm.semilla, paste0(codigo, "\nset.seed(",semilla,")"),codigo)
  return(codigo)
}

# Pagina de Resumen ---------------------------------------------------------------------------------------------------------

#Resumen Completo
cod.resum <- function(data = "datos"){
  return(paste0("summary(", data, ")"))
}

#Genera el resumen numerico de una variable
resumen.numerico <- function(data, variable) {
  datos.numericos <- list(
    Q1 = list(
      id = "q1", Label = tags$span(`data-id`="q1", tr("q1")), color = "green",
      Value = format(round(quantile(data[, variable], .25), 3), scientific = F)
    ),
    Mediana = list(
      id = "mediana", Label = tags$span(`data-id`="mediana", tr("mediana")),
      Value = format(round(median(data[, variable]), 3), scientific = F),
      color = "orange"),
    Q3 = list(
      id = "q3", Label = tags$span(`data-id`="q3", tr("q3")), color = "maroon",
      Value = format(round(quantile(data[, variable], .75), 3), scientific = F)
    ),
    Minimo = list(
      id = "minimo", Label = tags$span(`data-id`="minimo", tr("minimo")),
      Value = format(round(min(data[, variable]), 3), scientific = F),
      color = "red"),
    Promedio = list(
      id = "promedio", Label = tags$span(`data-id`="promedio", tr("promedio")),
      Value = format(round(mean(data[, variable]), 3), scientific = F),
      color = "blue"),
    Maximo = list(
      id = "maximo", Label = tags$span(`data-id`="maximo", tr("maximo")),
      Value = format(round(max(data[, variable]), 3), scientific = F),
      color = "purple"),
    DS <- list(
      id = "ds", Label = tags$span(`data-id`="ds", tr("ds")), color = "yellow",
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

# Pagina del Test de Normalidad ---------------------------------------------------------------------------------------------

#Codigo de la genracion de la curva normal (test de normalidad)
default.normal <- function(data = "datos", vars = NULL, color = "#00FF22AA", labelcurva = "Curva Normal"){
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0(
      "promedio <- mean(", data, "[, '", vars, "']) \n",
      "desviacion <- sd(", data, "[, '", vars, "']) \n",
      "values <- dnorm(", data, "[, '", vars, "'], mean = promedio, sd = desviacion) \n",
      "values <- c(values, hist(", data, "[, '", vars, "'],  plot = F)$density) \n",
      "hist(", data, "[, '", vars, "'], col = '", color, "', border=F, axes=F,\n",
      "  freq = F, ylim = range(0, max(values)), ylab = '',xlab = '",vars,"', \n",
      "  main = '", vars, "') \n",
      "axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "curve(dnorm(x, mean = promedio, sd = desviacion), add=T, col='blue', lwd=2)\n",
      "legend('bottom', legend = '", labelcurva, "', col = 'blue', lty=1, cex=1.5)"))
  }
}

fisher.calc <- function (x, na.rm = FALSE, ...) {
  if (!is.numeric(x)) {
    stop("argument 'x' is must be numeric")
  }
  if (na.rm)
    x <- x[!is.na(x)]
  nx <- length(x)

  sk <- sum((x - mean(x))^3/stats::sd(x)^3)/nx

  return(sk)
}

#Genera  la tabla de normalidad
default.calc.normal <- function(data = "datos", labelsi = "Positiva", labelno = "Negativa",labelsin = "Sin Asimetría") {
  return(paste0(
    "calc <- lapply(var.numericas(", data,"), function(i) fisher.calc(i)[1]) \n",
    "calc <- as.data.frame(calc) \n",
    "calc <- rbind(calc, lapply(calc, function(i) ifelse(i > 0, '", labelsi,
    "',\n  ifelse(i < 0, '", labelno, "', '", labelsin, "')))) \n",
    "calc <- t(calc)\ncalc"))
}

# Pagina de Dispersion ------------------------------------------------------------------------------------------------------

#Codigo del grafico de dispersion
default.disp <- function(data = "datos", vars = NULL, color = "#FF0000AA"){
  if(length(vars) < 2) {
    return(NULL)
  } else if(length(vars) == 2) {
    return(paste0("ggplot(data = ", data, ", aes(x = ", vars[1], ", y = ", vars[2], ", label = rownames(", data, "))) +
geom_point(color = '", color, "', size = 3) + geom_text(vjust = -0.7) + theme_minimal()"))
  } else{
    return(paste0("scatterplot3d(", data, "[, '", vars[1], "'], ", data, "[, '",
                  vars[2], "'], ", data, "[, '", vars[3], "'], pch = 16, color = '", color, "')"))
  }
}

# Pagina de Distribucion ----------------------------------------------------------------------------------------------------

#Llama a la funcion que crea la distribuccion numerica
def.code.num <- function(data = "datos", variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.numerico(", data, "[, ", variable, "], ", variable, ", color = ", color,")"))
}

#Llama a la funcion que crea la distribuccion categorica
def.code.cat <- function(data = "datos", variable, titulox = tr("cantidadcasos"), tituloy = tr("categorias")) {
  paste0("distribucion.categorico(", data, "[, '", variable,"']) + ",
         "labs(title = '", variable, "', x = '",titulox, "', y = '", tituloy, "')")
}

#Hace el grafico de la distribucion numerica
distribucion.numerico <- function(var, nombre.var, color){
  nf <- graphics::layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE),  height = c(3,1))
  par(mar=c(3.1, 3.1, 1.1, 2.1))
  hist(var, col = color, border=F, axes=F, main = nombre.var)
  axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  boxplot(var, col = color, boxcol = color, boxlty = 1, boxlwd = 3,
          boxwex = 1.5, edcol = color, medlty = 1, medlwd = 8, axes=F,
          medcol = color, whiskcol = color, whisklty = 3, staplecol = color,
          staplelty = 1, staplelwd = 3, horizontal=TRUE, outline=TRUE,
          frame=F, whisklwd = 2.5, outpch = 20, outcex = 1.5, outcol = 'red')
}

#Hace el grafico de la distribucion categorica
distribucion.categorico <- function(var) {
  colores <- sapply(levels(var),
                    function(i) rgb(runif(1), runif(1), runif(1), 0.8))
  data <- data.frame(label = levels(var), value = summary(var))
  ggplot(data, aes(label, value)) +
    geom_bar(stat = 'identity', fill = colores) +
    geom_text(aes(label = value, y = value), vjust = -0.5, size = 4) +
    theme_minimal()
}

# Pagina de Correlacion -----------------------------------------------------------------------------------------------------

#Calcula la matriz de correlacion
modelo.cor <- function(data = "datos"){
  return(paste0("correlacion <<- cor(var.numericas(", data, "))"))
}

#Codigo de la generacion de correlaciones
correlaciones <- function(metodo = 'circle', tipo = "lower"){
  return(paste0("corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black',
                tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
}

# Pagina de Poder Predictivo ------------------------------------------------------------------------------------------------

#Grafica el pairs
pairs.poder <- function(){
  return(paste0("pairs.panels(var.numericas(datos), bg = 'black', ellipses = FALSE, smooth = FALSE,
lm = TRUE, cex = 0.5,cex.main=0.1,
pch= 20, main='',
hist.col = gg_color_hue(3)[3], oma = c(1,1,1,1) )"))
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
  return(disp.modelos("prediccion.rl", modelo = tr("rll")))
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
  return(disp.modelos(paste0("prediccion.rlr.",rlr.type()), modelo = tr("rlr")))
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
  return(disp.modelos(paste0("prediccion.knn.",kernel), modelo = tr("knnl")))
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
  return(disp.modelos(paste0("prediccion.svm.",kernel), modelo = tr("svml")))
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
  return(disp.modelos("prediccion.dt", modelo = tr("dtl")))
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
  return(disp.modelos("prediccion.rf", modelo = tr("rfl")))
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
    "labs(title = '",tr("impVarRI"),"', y = '",tr("RI"),"', x = '')+\n",
    "scale_y_continuous(labels = scales::comma) + coord_flip() +\n",
    "theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')\n")
}

#Codigo de la dispersion de knn
boosting.disp <- function(type = "gaussian"){
  return(disp.modelos(paste0("prediccion.boosting.",type), modelo = tr("bl")))
}

# Pagina de NN ------------------------------------------------------------------------------------------------------------

#Crea el modelo NN
nn.modelo <- function(threshold = 0.01, stepmax = 1000, cant.cap = 2, ...){
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax <- ifelse(stepmax < 100, 100, stepmax)
  capas <- as.string.c(as.numeric(list(...)[1:cant.cap]), .numeric = TRUE)

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
  capas <- as.string.c(as.numeric(list(...)[1:cant.cap]), .numeric = TRUE)
  
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
  return(disp.modelos("prediccion.nn", modelo = tr("nn")))
}

nn.plot <- function(){
  paste0("plot(modelo.nn,,arrow.length = 0.1, rep = 'best', intercept = T,x.entry = 0.1, x.out = 0.9,\n\t",
         "information=F,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
         "dimension=15, radius = 0.2, fontsize = 10)")
}

# Pagina de TABLA COMPARATIVA -----------------------------------------------------------------------------------------------

# Pagina de REPORTE ---------------------------------------------------------------------------------------------------------

combinar.nombres <- function(n.modelos, n.modos){
  res <- c()
  for (modo in n.modos) {
    for (modelo in n.modelos) {
      res <- c(res,paste0(modelo,".",modo))
    }
  }
  return(res)
}

#Ordena el reporte
ordenar.reporte <- function(lista){
  nombres <- names(lista)
  orden <- c("carga.datos","na.delete","transformar.datos","segmentar.datos","resumen",
             nombres[grepl("normalidad.", nombres)],
             nombres[grepl("dispersion.", nombres)],
             nombres[grepl("dya.num.", nombres)],
             nombres[grepl("dya.cat.", nombres)],
             "correlacion","poder.pred",
             nombres[grepl("poder.cat.", nombres)],
             "poder.num",nombres[grepl("poder.den.", nombres)],
             "modelo.rl","pred.rl","disp.rl","ind.rl",
             combinar.nombres(c("modelo.rlr","posib.landa.rlr","coeff.landa.rlr","gcoeff.landa.rlr","pred.rlr","disp.rlr","ind.rlr"),
                              c("ridge", "lasso")),
             combinar.nombres(c("modelo.knn","pred.knn","disp.knn","ind.knn"),
                              c("optimal", "rectangular", "triangular","epanechnikov",
                                "biweight","triweight", "cos","inv","gaussian")),
             combinar.nombres(c("modelo.svm","pred.svm","disp.svm","ind.svm"),
                              c("linear", "polynomial", "radial","sigmoid")),
             "modelo.dt","modelo.dt.graf","pred.dt",
             "disp.dt","ind.dt","modelo.dt.rules",
             "modelo.rf","modelo.rf.graf",
             "pred.rf","disp.rf","ind.rf",
             nombres[grepl("modelo.rf.rules.", nombres)],
             combinar.nombres(c("modelo.b","modelo.b.imp","pred.b","disp.boosting","ind.b"),
                              c("gaussian", "laplace", "tdist")),
             "modelo.nn", "modelo.nn.graf", "pred.nn", "disp.nn", "ind.nn",
             "tabla.comparativa")

  orden <- c(orden, nombres[!(nombres %in% orden)])
  lista <- lista[orden]
  lista <- lista[!as.logical(lapply(lista, is.null))]
  return(lista)
}

def.reporte <- function(titulo = "Sin Titulo", nombre = "PROMiDAT", entradas) {
  codigo.usuario <- ""
  codigos <- env.report$codigo.reporte
  for (lista in codigos) {
    lista <- ordenar.reporte(lista)
    for (codigo in lista) {
      if(!is.data.frame(codigo)){
        codigo.usuario <- paste0(codigo.usuario, codigo)
      }
    }
  }
  paste0(
    "---\n", "title: '", titulo, "'\n", "author: '", nombre, "'\n",
    "date: ", Sys.Date(), "\n", "output:\n  word_document:\n",
    "    df_print: paged\n---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE,  fig.height = 10, fig.width = 15)\n",
    "```\n\n",
    "```{r message=FALSE, warning=FALSE}\n",
    "library(promises)\nlibrary(ggplot2)\nlibrary(neuralnet)\n",
    "library(corrplot)\n\nlibrary(scatterplot3d)\nlibrary(rattle)\n",
    "library(stringr)\nlibrary(gbm)\nlibrary(DT)\nlibrary(glmnet)\n",
    "library(kknn)\nlibrary(e1071)\nlibrary(rpart)\n",
    "library(rpart.plot)\nlibrary(randomForest)\nlibrary(ada)\nlibrary(xgboost)\n",
    "library(dplyr)\nlibrary(forcats)\n",
    "library(xtable)\n",
    "```\n\n", "```{r}\n", extract.code("var.numericas"), "\n\n",
    extract.code("var.categoricas"), "\n\n", extract.code("datos.disyuntivos"),
    "\n\n", extract.code("distribucion.numerico"), "\n\n",
    extract.code("distribucion.categorico"), "\n\n```",
    codigo.usuario)
}


recover.cat <- function(){
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

  lockBinding("cat",.BaseNamespaceEnv)
}

overwrite.cat <- function(){
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
      base:::.Internal(cat(as.list(conditionMessage(c)), file, sep, fill, labels, append))
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

# -------------------  Datos
datos <<- NULL
datos.originales <<- NULL
datos.prueba <<- NULL
datos.aprendizaje <<- NULL
variable.predecir <<- NULL
contador <<- 0
semilla <<- FALSE

nombres.modelos <<- c()

# -------------------  Estadisticas Basicas

correlacion <<- NULL
cod.poder.cat <- NULL
cod.poder.num <- NULL

# -------------------  Modelos

IndicesM <<- list()

# -------------------  RL

cod.rl.modelo <<-  NULL
cod.rl.pred <<-  NULL
cod.rl.ind <<- NULL

# -------------------  RLR

cod.rlr.modelo <<-  NULL
cod.rlr.pred <<-  NULL
cod.rlr.ind <<- NULL

cod.select.landa <<- NULL

# -------------------  KNN

cod.knn.modelo <<-  NULL
cod.knn.pred <<-  NULL
cod.knn.ind <<- NULL

knn.stop.excu <<- FALSE

# -------------------  SVM

cod.svm.modelo <<-  NULL
cod.svm.pred <<-  NULL
cod.svm.ind <<- NULL

# -------------------  DT

cod.dt.modelo <<-  NULL
cod.dt.pred <<-  NULL
cod.dt.ind <<- NULL

# -------------------  RF

cod.rf.modelo <<-  NULL
cod.rf.pred <<-  NULL
cod.rf.ind <<- NULL

rf.stop.excu <<- FALSE

# -------------------  BOOSTING

cod.b.modelo <<-  NULL
cod.b.pred <<-  NULL
cod.b.ind <<- NULL

# -------------------  NN

cod.nn.modelo <<-  NULL
cod.nn.pred <<-  NULL
cod.nn.ind <<- NULL

NN_EXECUTION <<- TRUE

mean.nn <<- NULL
sd.nn <<- NULL
mean.nn.np <<- NULL
sd.nn.np <<- NULL

# -------------------  Prediccion Nuevos

datos.originales.completos <<- NULL
datos.aprendizaje.completos <<- NULL
datos.prueba.completos <<- NULL

variable.predecir.pn <<- NULL
modelo.seleccionado.pn <<- NULL
contadorPN <<- 0
code.trans.pn <<- ""

modelo.nuevos <<- NULL
predic.nuevos <<- NULL

# -------------------  Reporte

env.report <<- new.env()
env.report$codigo.reporte <- list()

if(toupper(.Platform$OS.type) != "WINDOWS"){
  enc <<- "utf8"
}else{
  enc <<- "UTF-8"
}