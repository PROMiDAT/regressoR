
# In this file are the functions that generate the code as text

disp_models <- function(prediction, model_name, var_pred){
  paste0("plot_real_prediction(datos.prueba[,'",var_pred,"'], ", prediction,", '",model_name,"')")
}

#Genera el codigo para cargar datos
code_load <- function(row.names = T, path = NULL, sep = ";", sep.dec = ",", header = T, d.o = "datos.originales", d = "datos"){
  if(!is.null(path)){
    path <-  gsub("\\", "/", path, fixed=TRUE)
  }
  if(row.names){
    return(paste0( d.o ," <<- read.table('", path, "', header=", header, ", sep='", sep,
                   "', dec = '", sep.dec, "', row.names = 1) \n",d," <<- ",d.o))
  } else {
    return(paste0(d.o, "<<- read.table('", path, "', header=", header, ", sep='", sep,
                  "', dec = '", sep.dec,"') \n",d," <<- ",d.o))
  }
}

#Eliminar NAs
code_NA <- function(deleteNA = T, d.o = "datos.originales") {
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

# Pagina de Cargar y Transformar Datos --------------------------------------------------------------------------------------

#Genera el codigo para transformar datos
code.trans <- function(variable, nuevo.tipo, d.o = "datos.originales", d="datos"){
  if(nuevo.tipo == "categorico"){
    return(paste0(d,"[, '", variable, "'] <<- as.factor(",d,"[, '", variable, "'])"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0(d,"[, '", variable, "'] <<- as.numeric(sub(',', '.', ",d,"[, '", variable, "'], fixed = TRUE))"))
  } else {
    es.factor <- ifelse( eval(parse(text = paste0("class(",d.o,"[, variable]) %in% c('numeric', 'integer')"))),
                         paste0(d,"[, '", variable, "'] <<- as.factor(",d,"[, '", variable, "']) \n"), "")
    return(paste0(es.factor, d, " <<- disjunctive_data(",d,", '", variable,"')"))
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
datos.prueba <<- datos[-particion,]\ndatos.aprendizaje <<- datos[particion,]\nreal.val <<- datos.prueba[, '",variable.predecir,"', drop = F]")
  codigo <- ifelse(perm.semilla, paste0(codigo, "\nset.seed(",semilla,")"),codigo)
  return(codigo)
}

# Pagina de Resumen ---------------------------------------------------------------------------------------------------------

#Resumen Completo
cod.resum <- function(data = "datos"){
  return(paste0("summary(", data, ")"))
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
    "calc <- lapply(var_numerical(", data,"), function(i) fisher.calc(i)[1]) \n",
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
def.code.cat <- function(data = "datos", variable, titulox = translate("cantidadcasos"), tituloy = translate("categorias")) {
  paste0("distribucion.categorico(", data, "[, '", variable,"']) + ",
         "labs(title = '", variable, "', x = '",titulox, "', y = '", tituloy, "')")
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

pairs.poder <<- 
  "pairs.panels(var_numerical(datos), bg='black', ellipses=FALSE, smooth=FALSE, 
  lm=TRUE, cex=0.5, cex.main=0.1, pch=20, main='',
hist.col=gg_color_hue(3)[3], oma=c(1,1,1,1) )"