

env_report <- new.env()
env_report$codigo.reporte <- list()

#' get_env_report
#'
#' @description gets the environment where the list is found with the report
#'
#' @export
#'
#' @examples
#' e <- get_env_report()
#' e$codigo.reporte
#' 
get_env_report <- function(){
  return(env_report)
}

#' clean_report
#'
#' @description clean the full report
#'
#' @export
#'
#' @examples
#' new_report(iris, 'iris')
#' get_report()
#' clean_report()
#' get_report()
#' 
clean_report <- function(){
  env_report$codigo.reporte <- list()
}

#' get_report
#'
#' @description gets the list of report values
#'
#' @export
#'
#' @examples
#' get_report()
#' 
get_report <- function(){
  return(env_report$codigo.reporte)
}

#' len_report
#'
#' @description gets the size of the current report.
#'
#' @keywords internal
#' 
len_report <- function(){
  length(env_report$codigo.reporte)
}

#' chunk
#' 
#' @description locks the data into a rmarkdown chunk.
#'
#' @param content the content to be inserted.
#'
#' @keywords internal
#' 
chunk <- function(content = ""){
  paste0("```{r}\n", content, "\n```")
}

#' new_report
#' 
#' @description creates a new report section within the list. All new reports section store data and data names as headers.
#'
#' @param data the data that is stored in the report list
#' @param name the name of the stored data
#'
#' @export
#'
#' @examples
#' new_report(iris, "iris")
#' get_report()
#' clean_report()
#' 
new_report <- function(data, name = ""){
  n <- len_report() + 1
  env_report$codigo.reporte[[n]] <- list(datos.originales = data)
  env_report$codigo.reporte[[n]][["carga.datos"]] <- paste0("\n# Carga de Datos (",name,")",
                                                             "\n```{r}\ndatos.originales <<- codigo.reporte[[",n,"]]$datos.originales\n",
                                                             "datos <<- datos.originales\n```\n```{r}\nhead(datos)\n```\n```{r}\nstr(datos)\n```\n",
                                                             "```{r}\nIndicesM <<- list()\n```\n")
}

#' insert_report
#' 
#' @description inserts an element in the report in the current section.
#'
#' @param id a string with the key of what is inserted in the report.
#' @param title the title of the content, if there is no title is NA.
#' @param ... the content to be inserted.
#' @param interpretation a logical value indicating whether a label has to be inserted for interpretation.
#' @param is.chunk a logical value indicating whether the content has to be enclosed in a chunk.
#' @param add a logical value indicating if the content has to be added to what is a before.
#' 
#' @export
#'
#' @examples
#' new_report(iris, "iris")
#' insert_report("1_part", 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' clean_report()
#' 
insert_report <- function(id, title = NA, ... ,  interpretation =  TRUE, is.chunk = TRUE, add = FALSE){
  n <- len_report()
  content <- paste0(...)
  title <- ifelse(is.na(title), "\n", paste0("## ", title, "\n"))
  content <- ifelse(is.chunk, chunk(content), content)
  inter <- ifelse(interpretation, "\n\n#### <Interpretaci\u00F3n>\n\n", "")
  if(!add){
    env_report$codigo.reporte[[n]][[id]] <- ifelse(interpretation, paste0(title, content, inter), paste0(title, content))
  }else{
    aux <- env_report$codigo.reporte[[n]][[id]]
    env_report$codigo.reporte[[n]][[id]] <- paste0(aux , "\n", title, content, inter)
  }
}


#' remove_report_elem
#' 
#' @description removes an element from the report according to its key in the current section.
#'
#' @param id a string with the key of what is removed in the report.
#'
#' @export
#'
#' @examples
#' new_report(iris, 'iris')
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' remove_report_elem('1_part')
#' get_report()
#' clean_report()
#' 
remove_report_elem <- function(id){
  n <- len_report()
  env_report$codigo.reporte[[n]][[id]] <- NULL
}

#' new_section_report
#'
#' @description Creates a new section in the report, this way you can overwrite keys and delete an element only affects the current section.
#'
#' @export
#'
#' @examples
#' new_report(iris, 'iris')
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' 
#' remove_report_elem('1_part')
#' get_report()
#' 
#' new_section_report()
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' 
#' new_section_report()
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' 
#' remove_report_elem('1_part')
#' get_report()
#' 
#' clean_report()
#' 
new_section_report <- function(){
  n <- len_report() + 1
  env_report$codigo.reporte[[n]] <- list()
}



order_report <- function(list_report, order){
  nombres <- names(list_report)
  order <- c(order, nombres[!(nombres %in% order)])
  list_report <- list_report[order]
  list_report <- list_report[!as.logical(lapply(list_report, is.null))]
  return(list_report)
}

word_report <- function(title = "Sin Titulo", name = "PROMiDAT", order_by = c("")) {
  codigo.usuario <- ""
  codigos <- env_report$codigo.reporte
  
  for (list_r in codigos) {
    list_r <- order_report(list_r, order_by)
    for (codigo in list_r) {
      if(!is.data.frame(codigo)){
        codigo.usuario <- paste0(codigo.usuario, codigo)
      }
    }
  }
  
  paste0(
    "---\n", "title: '", title, "'\n", "author: '", name, "'\n",
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
    "```\n\n",
    codigo.usuario)
}






