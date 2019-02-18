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
  if(package_version(unclass(packageDescription("shiny"))$Version) < package_version("1.2.0") ){
    installed.packages("shiny")
  }
  rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
  Sys.setenv("LANGUAGE" = "ES")
  if(toupper(.Platform$OS.type) != "WINDOWS"){
    options(encoding = "utf8")
  }else{
    options(encoding = "UTF-8")
  }
  shiny::runApp(appDir = system.file("application", package = "regresoR"), launch.browser = TRUE)
}
