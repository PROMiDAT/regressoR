#' information_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_information_page_ui <- function(id){
  ns <- NS(id)
  
  page.info <- tabItem(tabName = "acercaDe",
                       img(src="img/Logo.png", style="padding-bottom:20px;margin-left: auto;margin-right: auto;display: block;width: 50%;"),
                       infoBoxPROMiDAT(labelInput("copyright"), "PROMiDAT S.A.", icon = icon("copyright")),
                       infoBoxPROMiDAT(labelInput("info"), tags$a( href="https://www.promidat.com/", style = "color:white;",
                                                                   target = "_blank", "https://www.promidat.com"), icon = icon("info")),
                       infoBoxPROMiDAT(labelInput("version"), "2.0.0", icon = icon("file-code-o")))
  
  
  tagList(
    page.info
  )
}
    
#' information_page Server Function
#'
#' @noRd 
mod_information_page_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_information_page_ui("information_page_ui_1")
    
## To be copied in the server
# callModule(mod_information_page_server, "information_page_ui_1")
 