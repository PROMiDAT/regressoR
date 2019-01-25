#' Start regresoR
#' @title This function will start regresoR
#' @return Nothing
#' @description An interactive Shiny application for data prediction.
#' @details This starts the regresoR application on the user's local computer.
#' @keywords predictoR
#' @examples
#' \dontrun{
#'  if(interactive()){
#'    init_regresor()
#'  }
#'}
init_regresor <- function(){
  rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
  info.sys <- Sys.info()
  if(is.null(info.sys)){
    info.sys <- .Platform$OS.type
  }
  Sys.setenv("LANGUAGE" = "ES")
  if(toupper(info.sys) != "WINDOWS"){
    options(encoding = "utf8")
  }else{
    options(encoding = "UTF-8")
  }
  shiny::runApp(appDir = system.file("application", package = "regresoR"), launch.browser = TRUE)
}
