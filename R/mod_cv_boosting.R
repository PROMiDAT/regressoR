#' cv_boosting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_boosting_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' cv_boosting Server Functions
#'
#' @noRd 
mod_cv_boosting_server <- function(input, output, session, updateData, codedioma){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_cv_boosting_ui("cv_boosting_1")
    
## To be copied in the server
# mod_cv_boosting_server("cv_boosting_1")
