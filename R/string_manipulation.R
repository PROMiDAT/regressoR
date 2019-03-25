
#' exe
#' 
#' @description Concat and execute a text in R
#'
#' @param ... one or more texts to be concatenated and executed
#' 
#' @return the result of the execute
#' @export
#'
#' @examples
#' exe("5+5")
#' exe("5","+","5")
#' exe("plot(iris$Species)")
#' 
exe <- function(...){
  eval(parse(text = paste0(...)))
}

#' extract.code
#' 
#' @description Gets the code of a function in text form
#'
#' @param funcion the name of the function to be extracted
#'
#' @return the code in text form
#' @export
#'
#' @examples
#' extract.code("cat")
#' extract.code("plot")
#' 
#' parse(text = extract.code("plot"))
#' 
extract.code <- function(funcion) {
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



