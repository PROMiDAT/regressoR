
# In this file are the functions that generate the code as text

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
  paste0("plot_real_prediction(datos.prueba[,'",var_pred,"'], ", prediction,", '",model_name,"')")
}

#' code_load
#' 
#' @description generates data reading code.
#'
#' @param row.names a logical value indicating whether the data has row names.
#' @param path the path of the file.
#' @param sep the column separator in the file.
#' @param sep.dec the decimal separator in the file.
#' @param header a logical value indicating whether the file has a header.
#' @param d.o the name of the original data.
#' @param d the name of the current data.
#'
#' @export
#'
#' @examples
#' code_load(TRUE, "MY/PATH/FILE.csv")
#' 
code_load <- function(row.names = TRUE, path = NULL, sep = ";", sep.dec = ",", header = TRUE, d.o = "datos.originales", d = "datos"){
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

#' code_NA
#' 
#' @description creates the code that imputes the NAs data or removes them.
#'
#' @param deleteNA a logical value indicating whether the NAs have to be eliminated or whether they have to be imputed. If TRUE then the NAs are eliminated, otherwise the data is imputed.
#' @param d.o the name of the original data.
#'
#' @export
#'
#' @examples
#' code_NA(TRUE, 'iris')
#' code_NA(FALSE, 'iris')
#' 
code_NA <- function(deleteNA = TRUE, d.o = "datos.originales") {
  ifelse(deleteNA, paste0(d.o, " <<- na.omit(",d.o,")\n"),
         paste0("Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
                "for (variable in colnames(",d.o,")) {\n",
                "  if(any(is.na(",d.o,"[, variable]))){\n",
                "   ifelse(class(",d.o,"[, variable]) %in% c('numeric', 'integer'),\n",
                "           ",d.o,"[, variable][is.na(",d.o,"[, variable])] <<- \n",
                "                                              mean(",d.o,"[, variable], na.rm = TRUE),\n",
                "           ",d.o,"[, variable][is.na(",d.o,"[, variable])] <<- \n",
                "                                     Mode(",d.o,"[, variable]))",
                "\n   }\n}"))
}

#' code_transf
#' 
#' @description generate code to transform data.
#'
#' @param variable the name of the variable to be converted.
#' @param new.type the new type of the variable. Can be categorical, numerical or disjunctive. ('categorico', 'numerico', 'disyuntivo')
#' @param d.o the name of the original data.
#' @param d the name of the current data.
#'
#' @export
#' 
code_transf <- function(variable, new.type, d.o = "datos.originales", d="datos"){
  if(new.type == "categorico"){
    return(paste0(d,"[, '", variable, "'] <<- as.factor(",d,"[, '", variable, "'])"))
  } else if(new.type == "numerico") {
    v <- as.character(exe(d,"[, '", variable, "']"))
    if(all(grepl("^[[:digit:]]+((\\.|,)[[:digit:]]+)*$", v))){
      return(paste0(d,"[, '", variable, "'] <<- as.numeric(sub(',', '.', ",d,"[, '", variable, "'], fixed = TRUE))"))
    }else{
      return(paste0(d,"[, '", variable, "'] <<- as.numeric(",d,"[, '", variable, "'])"))
    }
  } else {
    es.factor <- ifelse( eval(parse(text = paste0("class(",d.o,"[, variable]) %in% c('numeric', 'integer')"))),
                         paste0(d,"[, '", variable, "'] <<- as.factor(",d,"[, '", variable, "']) \n"), "")
    return(paste0(es.factor, d, " <<- disjunctive_data(",d,", '", variable,"')"))
  }
}

#' code_deactivate
#' 
#' @description creates the code that deactivates the selected variables of the data.
#'
#' @param variables the name of the variables to be deactivated.
#' @param d the name of the current data.
#'
#' @export
#'
#' @examples
#' iris2 <- iris
#' x <- code_deactivate('Species', 'iris2')
#' exe(x)
#' 
code_deactivate <- function(variables, d = "datos"){
  return(paste0(d, " <<- subset(",d,", select = -c(", paste(variables, collapse = ","), "))"))
}

#' partition_code
#'
#' @description creates the partition code for testing and learning data.
#'
#' @param data the name of the current data.
#' @param p the percentage of data for the learning data.
#' @param variable  the name of the variable to be predicted.
#' @param semilla a number with the random seed.
#' @param perm.semilla a logical value indicating whether the random seed should be used.
#'
#' @export
#'
#' @examples
#' x <- partition_code('iris', 75, 'Species', 555, TRUE)
#' exe(x)
#' 
partition_code <- function(data = "datos", p = 50, variable = NULL, semilla = 5, perm.semilla = FALSE){
  semilla <- ifelse(is.numeric(semilla), semilla, 5)
  codigo <- ifelse(perm.semilla, paste0("set.seed(",semilla,")"), "rm(.Random.seed, envir = globalenv())")
  codigo <- paste0(codigo,"\nvariable.predecir <<- '",variable,"'\nparticion <- sample(1:nrow(",data,"),size = nrow(",data,")*",p/100,", replace = FALSE)\n",
                  "datos.prueba <<- ",data,"[-particion,]\ndatos.aprendizaje <<- ",data,"[particion,]\nreal.val <<- datos.prueba[, '",variable,"', drop = FALSE]")
  codigo <- ifelse(perm.semilla, paste0(codigo, "\nset.seed(",semilla,")"),codigo)
  return(codigo)
}

#' code_summary
#'
#' @description creates the code for the basic summary of variables.
#'
#' @param data the name of the current data.
#'
#' @export
#'
#' @examples
#' x <- code_summary('iris')
#' exe(x)
#' 
code_summary <- function(data = "datos"){
  return(paste0("summary(", data, ")"))
}

#' normal_default
#' 
#' @description generates the code of the normality test.
#'
#' @param data the name of the current data.
#' @param vars the variable for analysis. It has to be numeric.
#' @param color the color of the histogram.
#' @param labelcurva label for the curve.
#'
#' @export
#'
#' @examples
#' x <- normal_default('iris', 'Sepal.Length')
#' exe(x)
#' 
normal_default <- function(data = "datos", vars = NULL, color = "#00FF22AA", labelcurva = "Curva Normal"){
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0(
      "promedio <- mean(", data, "[, '", vars, "']) \n",
      "desviacion <- sd(", data, "[, '", vars, "']) \n",
      "values <- dnorm(", data, "[, '", vars, "'], mean = promedio, sd = desviacion) \n",
      "values <- c(values, hist(", data, "[, '", vars, "'],  plot = FALSE)$density) \n",
      "hist(", data, "[, '", vars, "'], col = '", color, "', border=FALSE, axes=FALSE,\n",
      "  freq = FALSE, ylim = range(0, max(values)), ylab = '',xlab = '",vars,"', \n",
      "  main = '", vars, "') \n",
      "axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "curve(dnorm(x, mean = promedio, sd = desviacion), add=TRUE, col='blue', lwd=2)\n",
      "legend('bottom', legend = '", labelcurva, "', col = 'blue', lty=1, cex=1.5)"))
  }
}

#' default_calc_normal
#'
#' @description generates the code that creates the asymmetry table.
#'
#' @param data the name of the current data.
#' @param label.yes the label for when the asymmetry is positive.
#' @param label.no the label for when the asymmetry is negative.
#' @param label.without the label for when there is no asymmetry.
#'
#' @export
#'
#' @examples
#' x <- default_calc_normal('iris')
#' exe(x)
#' 
default_calc_normal <- function(data = "datos", label.yes = "Positiva", label.no = "Negativa", label.without = "Sin Asimetría") {
  return(paste0(
    "calc <- lapply(var_numerical(", data,"), function(i) fisher_calc(i)[1]) \n",
    "calc <- as.data.frame(calc) \n",
    "calc <- rbind(calc, lapply(calc, function(i) ifelse(i > 0, '", label.yes,
    "',\n  ifelse(i < 0, '", label.no, "', '", label.without, "')))) \n",
    "calc <- t(calc)\ncalc"))
}


#' default_disp
#'
#' @param data the name of the current data.
#' @param vars a vector of length 2 or 3 with the names of the variables for the graph.
#' @param color the color of the dots on the chart.
#'
#' @export
#'
#' @examples
#' x <- default_disp('iris', c('Sepal.Length', 'Sepal.Width'))
#' exe(x)
#' #x <- default_disp('iris', c('Sepal.Length', 'Sepal.Width', 'Petal.Length'))
#' #exe(x)
default_disp <- function(data = "datos", vars = NULL, color = "#FF0000AA"){
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

#' pairs_power
#'
#' @export
#'
#' @examples
#' #x <- pairs_power('iris')
#' #exe(x)
pairs_power <- function(data = "datos"){ 
  paste0("pairs.panels(var_numerical(",data,"), bg='black', ellipses=FALSE, smooth=FALSE,",
         "lm=TRUE, cex=0.5, cex.main=0.1, pch=20, main='',",
         "hist.col = gg_color_hue(3)[3], oma=c(1,1,1,1) )")
}



#Llama a la funcion que crea la distribuccion numerica
def_code_num <- function(data = "datos", variable, color = 'red'){
  return(paste0("numerical_distribution(", data, "[, ", variable, "], ", variable, ", color = ", color,")"))
}

#Llama a la funcion que crea la distribuccion categorica
def.code.cat <- function(data = "datos", variable, titulox = translate("cantidadcasos"), tituloy = translate("categorias")) {
  paste0("distribucion.categorico(", data, "[, '", variable,"']) + ",
         "labs(title = '", variable, "', x = '",titulox, "', y = '", tituloy, "')")
}

