#' r_numerico UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_r_numerico_ui <- function(id){
  ns <- NS(id)
  
  # NUMERICAL SUMMARY PAGE --------------------------------------------------------------------------------------------------
  
  full.summary.table <- box(title = labelInput("resumen"), status = "primary", width = 7, solidHeader = TRUE, collapsible = TRUE,
                            DT::dataTableOutput(ns("resumen.completo")), hr(),
                            aceEditor(ns("fieldCodeResum"), mode = "r", theme = "monokai", value = "", height = "5vh",  readOnly = T))
  
  variable.summary.table <- box(title = labelInput("resumenvar"), status = "primary", width = 5, solidHeader = TRUE, collapsible = TRUE,
                                selectInput(inputId = ns("sel.resumen"), label = labelInput("selvar"), choices =  ""),
                                fluidRow(uiOutput(ns("resumen"))))
  
  page.numerical.summary <- tabItem(tabName = "resumen",
                                    fluidRow(full.summary.table,
                                             variable.summary.table ))
  
  
  tagList(
    page.numerical.summary
  )
}
    
#' r_numerico Server Function
#'
#' @noRd 
mod_r_numerico_server <- function(input, output, session,updateData){
  ns <- session$ns
 
  
  # SUMMARY PAGE ----------------------------------------------------------------------------------------------------------
  
  # Some code fields that are not parameter-dependent
  updateAceEditor(session, "fieldCodeResum", value = code_summary())
  
  
  #Update the options with the names of the data columns
  observeEvent(updateData$datos, {
    updateSelectInput(session, "sel.resumen", choices = colnames_empty(updateData$datos))
    
    
    # Change the table with the summary on the summary page
    output$resumen.completo <- DT::renderDataTable({ 
      #insert_report("resumen","Resumen Num\u00E9rico", "summary(datos)")
      data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
    }, options = list(dom = "ft", scrollX = TRUE), rownames = F)
    
    
    # Change summary tables by variable
    output$resumen <- renderUI({
      if (input$sel.resumen %in% colnames(var_numerical(datos))){
        numerical_summary(datos, input$sel.resumen)
      }else{
        categorical_summary(datos, input$sel.resumen)
      }
    })
  })
  
}
    
## To be copied in the UI
# mod_r_numerico_ui("r_numerico_ui_1")
    
## To be copied in the server
# callModule(mod_r_numerico_server, "r_numerico_ui_1")
 
