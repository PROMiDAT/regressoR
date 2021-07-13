#' Predictive_Power UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Predictive_Power_ui <- function(id){
  ns <- NS(id)
  
  # PREDICTIVE POWER PAGE ---------------------------------------------------------------------------------------------------
  
  code.power.num <- list(h3(labelInput("codigo")), hr(),
                         aceEditor(ns("fieldCodePoderNum"), mode = "r", theme = "monokai",
                                   value = "", height = "7vh", readOnly = F, autoComplete = "enabled"))
  
  
  tabs.power.num <- tabsOptions(buttons = list(icon("terminal")), widths = 100, heights = 55,
                                tabs.content = list(code.power.num))
  
  power.plot.pairs <- tabPanel(title = labelInput("pares"), value = "predpares",
                               plotOutput(ns('plot.pairs.poder'), height = "75vh"))
  
  pagina.poder <- tabItem(tabName = "poderPred",
                          tabBox(id = ns("BoxPodPred"), width = NULL,
                                 power.plot.pairs,
                                 tabs.power.num))
  
  
  
  tagList(
    pagina.poder
  )
}
    
#' Predictive_Power Server Function
#'
#' @noRd 
mod_Predictive_Power_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  # Show the graph of numerical predictive power
  observeEvent(updateData$datos.aprendizaje,{
    output$plot.pairs.poder <- renderPlot({
      tryCatch({
        updateAceEditor(session, "fieldCodePoderNum", value = "pairs_power(datos)")
        if (ncol(var_numerical(updateData$datos)) >= 2) {
          if(ncol(var_numerical(updateData$datos)) <= 25){
            return(pairs_power(updateData$datos))
          }else{
            showNotification(translate("bigPlot",updateData$idioma), duration = 10, type = "message")
            return(NULL)
          }
        }else{
          return(NULL)
        }
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e),duration = 10,type = "error")
        return(NULL)
      })
    })
  })
  
}
    
## To be copied in the UI
# mod_Predictive_Power_ui("Predictive_Power_ui_1")
    
## To be copied in the server
# callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1")
 
