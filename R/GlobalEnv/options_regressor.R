my_options <- new.env()
my_options$ops <- list(language  = "es",
                       rd.mode   = 0,
                       rlr.alpha = 1,
                       exe.envir = NULL)



#' options_regressor
#'
#' @param ... any options can be defined, using name = value or a character string holding an option name.
#' 
#' @export
#' 
#' @examples
#' options_regressor("language")
#' options_regressor(language = "en")
#' options_regressor("language")
#' 
options_regressor <- function(...){
  if(missing(...)){
    return(my_options$ops)
  }else{
    .args <- list(...)
    .names <- names(.args)
    if(suppressWarnings(all(!is.na(as.numeric(.names))))){
      my_options$ops[unlist(.args)]
    }else{
      my_options$ops[.names] <- unlist(.args)
    }
  }
}

options_regressor(language = "es")