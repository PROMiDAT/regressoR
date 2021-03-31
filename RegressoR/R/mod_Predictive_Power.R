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
  
  code.power.num <- list(h4(labelInput("codigo")), hr(),
                         code_field(runid = ns("run.code.poder.num"), fieldid = ns("fieldCodePoderNum"), height = "16vh"))
  
  
  tabs.power.num <- tabsOptions(buttons = list(icon("terminal")), widths = 100, heights = 55,
                                tabs.content = list(code.power.num))
  
  power.plot.pairs <- tabPanel(title = labelInput('pares'), value = "predpares",
                               plotOutput(ns('plot.pairs.poder'), height = "55vh"))
  
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
mod_Predictive_Power_server <- function(input, output, session, updateData, updatePlot, disp.ranges){
  ns <- session$ns
 
  # PREDICTIVE POWER PAGE -------------------------------------------------------------------------------------------------
  
  # Show the graph of numerical predictive power
  observeEvent(updateData$datos.aprendizaje,{
    output$plot.pairs.poder <- renderPlot({
      tryCatch({
        cod.poder.num <<- updatePlot$poder.num
        updateAceEditor(session, "fieldCodePoderNum", value = cod.poder.num)
        if (ncol(var_numerical(datos)) >= 2) {
          if(ncol(var_numerical(datos)) <= 25){
            res <- isolate(exe(cod.poder.num))
            #insert_report("poder.num","Poder Predictivo Variables Num\u00E9ricas", cod.poder.num)
            return(res)
          }else{
            showNotification(translate("bigPlot"), duration = 10, type = "message")
            return(NULL)
          }
        }else{
          error_variables( T)
        }
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
  })
  
  # Execute the graphic code
  observeEvent(input$run.code.poder.num, {
    if(input$fieldCodePoderNum != "") {
      updatePlot$poder.num <- input$fieldCodePoderNum
    } else {
      updatePlot$poder.num <- pairs_power()
    }
  })
  
  # Change the graphic code
  observeEvent(updateData$datos.aprendizaje,{
    updatePlot$poder.num <- pairs_power()
  }, priority = 3)
  
  
}
    
## To be copied in the UI
# mod_Predictive_Power_ui("Predictive_Power_ui_1")
    
## To be copied in the server
# callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1")
 
