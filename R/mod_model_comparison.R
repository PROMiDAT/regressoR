#' model_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_model_comparison_ui <- function(id){
  ns <- NS(id)
  
  model.selector <- checkboxGroupButtons(ns("select.models"), labelInput("selectMod"), c(" ---- " = "NoDisponible"),
                                         size = "sm", status = "primary",
                                         checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                          no = icon("remove", lib = "glyphicon")))
  
  comparison.options <- list(fluidRow(column(width = 10,h4(labelInput("opciones")))),
                             hr(),
                             fluidRow(column(model.selector, width = 12)))
  
  
  tabs.comparison  <- tabsOptions(buttons = list(icon("gear")), widths = c(100), heights = c(88),
                                  tabs.content = list(comparison.options))
  
  table.comparison.panel <- tabPanel(title = labelInput("tablaComp"),
                                     DT::dataTableOutput(ns("TablaComp"), height="70vh"))
  
  page.comparison <- tabItem(tabName = "comparar",
                             tabBox(id = ns("BoxCom"), width = NULL, height ="80%",
                                    table.comparison.panel,
                                    tabs.comparison))
  
  tagList(
    page.comparison
  )
}
    
#' model_comparison Server Function
#'
#' @noRd 
mod_model_comparison_server <- function(input, output, session, updateData, updatePlot){
  ns <- session$ns
 
  observeEvent(updateData$IndicesM,{
    update_comparative_selector()
  })
  
  
  #Muestra la tabla comparativa.
  output$TablaComp <- DT::renderDataTable({
    graficar <- updatePlot$tablaCom
    if (!is.null(datos.aprendizaje)) {
      
      DT::datatable(comparative_table(input$select.models,updateData$IndicesM),
                    selection = "none", editable = FALSE,
                    options = list(dom = "frtip", pageLength = 9, buttons = NULL))
    }
  },server = FALSE)
  
  # Updates the selectors in the comparison table page
  update_comparative_selector <- function(){
    nombres <- models_mode(updateData$IndicesM)
    shinyWidgets::updateCheckboxGroupButtons(session,"select.models",choices = sort(nombres),selected = sort(nombres),
                                             status = "primary",checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                                 no = icon("remove", lib = "glyphicon")))
  }
  
}
    
## To be copied in the UI
# mod_model_comparison_ui("model_comparison_ui_1")
    
## To be copied in the server
# callModule(mod_model_comparison_server, "model_comparison_ui_1")
 
