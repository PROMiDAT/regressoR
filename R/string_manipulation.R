
#' exe
#' 
#' @description Concat and execute a text in R
#'
#' @param ... one or more texts to be concatenated and executed
#' @param envir the environment in which expr is to be evaluated
#' 
#' @return the result of the execute
#' @export
#'
#' @examples
#' exe("5+5")
#' exe("5","+","5")
#' exe("plot(iris$Species)")
#' 
exe <- function(..., envir = parent.frame()){
  eval(parse(text = paste0(...)), envir = envir)
}

#' extract_code
#' 
#' @description Gets the code of a function in text form
#'
#' @param funcion the name of the function to be extracted
#'
#' @return the code in text form
#' @export
#'
#' @examples
#' extract_code("cat")
#' extract_code("plot")
#' 
#' parse(text = extract_code("plot"))
#' 
extract_code <- function(funcion) {
  code <- paste(head(exe(funcion), 100), collapse = "\n")
  code <- paste(funcion, "<-", code)
  return(code)
}

#' as.string.c
#' 
#' @description Creates a string representative of a vector
#'
#' @param vect a vector
#' @param quote a logical value. If TRUE, the values on the vector will be surrounded by quotes.
#' 
#' @return a text
#' @export
#' 
#' @examples
#' as.string.c(c("A", "B", "C"))
#' as.string.c(c(5, 6, 7))
#' as.string.c(c(5, 6, 7), quote = FALSE)
#' as.string.c(iris$Species)
#'
as.string.c <- function(vect, quote = TRUE){
  if(quote){
    return(paste0("c('",paste0(vect, collapse = "','"),"')"))
  }
  else{
    return(paste0("c(",paste0(vect, collapse = ","),")"))
  }
}

load("inst/extdata/translation.bin") # Load translation.bin (dictionary to change language)
enc <- ifelse(toupper(.Platform$OS.type) != "WINDOWS", "utf8", "UTF-8")

#' translate
#' 
#' @description translates text id into current language
#' 
#' @param text the id for the text.
#' @param language the language to choose. It can be "es" or "en".
#' 
#' @details Use data(dictionary) to see the data
#' 
#' @export
#' @examples
#' translate("knnl")
#' translate("knnl", "en")
#' 
translate <- function(text, language = "es") {
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[language]]), s, translation[[s]][[language]])
    Encoding(elem) <- enc
    elem
  }, USE.NAMES = F)
}


#' models_mode
#'
#' @param list.names a list whose names function as keys for \code{\link{translate}}. The names have to have the key-mode form.
#' @param language the language to choose. It can be "es" or "en".
#' 
#' @return a vector with the names
#' @export
#'
#' @examples
#' x <- list('knnl-mode1' = 1, 'knnl-mode2' = 2, 'knnl-mode2' = 5)
#' models_mode(x)
#' 
models_mode <- function(list.names = list(), language = "es"){
  if(length(list.names) == 0) {
    return("---X---")
  }
  nombres <- c()
  for (nom in names(list.names)){
    nom.aux <- unlist(strsplit(nom, "-"))
    nombres <- c(nombres,ifelse(length(nom.aux) == 1,
                                translate(nom.aux, language),
                                paste0(translate(nom.aux[1], language),"-",nom.aux[2])))
  }
  return(nombres)
}



