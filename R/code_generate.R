
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
code_load <- function(row.names = TRUE, path = NULL, sep = ";", sep.dec = ",", header = T, d.o = "datos.originales", d = "datos"){
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
                "                                              mean(",d.o,"[, variable], na.rm = T),\n",
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
#' @examples
#' code_transf('Species', 'numerico', 'iris', 'iris')
#' code_transf('Species', 'disyuntivo', 'iris', 'iris')
#' 
code_transf <- function(variable, new.type, d.o = "datos.originales", d="datos"){
  if(new.type == "categorico"){
    return(paste0(d,"[, '", variable, "'] <<- as.factor(",d,"[, '", variable, "'])"))
  } else if(new.type == "numerico") {
    return(paste0(d,"[, '", variable, "'] <<- as.numeric(sub(',', '.', ",d,"[, '", variable, "'], fixed = TRUE))"))
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
#' code_deactivate('Species', 'iris')
#' 
code_deactivate <- function(variables, d = "datos"){
  return(paste0(d, " <<- subset(",d,", select = -c(", paste(variables, collapse = ","), "))"))
}


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

#Resumen Completo
cod.resum <- function(data = "datos"){
  return(paste0("summary(", data, ")"))
}
