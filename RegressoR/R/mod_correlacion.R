#' correlacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_correlacion_ui <- function(id){
  ns <- NS(id)
  
  # CORRELATIONS PAGE -------------------------------------------------------------------------------------------------------
  
  cor.options <-list(h4(labelInput("opciones")), hr(),
                     selectInput(inputId = ns("cor.metodo"), label = labelInput("selmetodo"),
                                 choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                     selectInput(inputId = ns("cor.tipo"), label = labelInput("seltipo"), choices =  c("lower", "upper", "full")))
  
  cor.code <- list(h4(labelInput("codigo")), hr(),
                   aceEditor(ns("fieldModelCor"), height = "6vh", mode = "r", theme = "monokai", value = "", readOnly = T),
                   code_field(ns("run.code.cor"),ns("fieldCodeCor"), height = "7vh"))
  
  cor.tabs <- tabsOptions(heights = c(48, 63),
                          tabs.content = list(cor.options, cor.code))
  
  correlation.plot <- tabPanel(title = labelInput("correlacion"), value = "correlacion", plotOutput(ns('plot.cor'), height = "70vh"))
  
  results.table.correlations <- tabPanel(title = labelInput("resultados"), value = "cor.salida", verbatimTextOutput(ns("txtcor")))
  
  page.correlations <- tabItem(tabName = "correlacion",
                               tabBox(id = ns("tabCor"), width = NULL,
                                      correlation.plot,
                                      results.table.correlations,
                                      cor.tabs))
  
  
  tagList(
    page.correlations
  )
}
    
#' correlacion Server Function
#'
#' @noRd 
mod_correlacion_server <- function(input, output, session, updateData, updatePlot, disp.ranges){
  ns <- session$ns
 
  # CORRELATION PAGE ------------------------------------------------------------------------------------------------------
  
  # Some code fields that are not parameter-dependent
  updateAceEditor(session, "fieldModelCor" , value = cor_model())
  
  # Executes the code of correlations
  run_cor_model <- function() {
    tryCatch({
      isolate(exe(text = cor_model()))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
  }
  
  # Show the correlation graph
  observeEvent(c(updateData$datos, input$fieldModelCor), {
    run_cor_model()
    
    output$plot.cor <- renderPlot({
      tryCatch({
        cod.cor <<- updatePlot$cor
        res <- isolate(exe(cod.cor))
        updateAceEditor(session, "fieldCodeCor", value = cod.cor)
        insert_report("correlacion", "Correlaci\u00F3n", cor_model(),"\n", cod.cor)
        return(res)
      }, error = function(e) {
        if (ncol(var_numerical(datos)) == 0){
          error_variables( T)
        }else{
          showNotification(paste0("ERROR EN Correlacion: ", e),
                           duration = 10,
                           type = "error")
          return(NULL)
        }
      })
    })
  })
  
  # Change the graphic code
  observeEvent(input$run.code.cor, {
    updatePlot$cor <- input$fieldCodeCor
  })
  
  # Executes the code when parameters change
  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlations_plot(method = input$cor.metodo, type = input$cor.tipo)
  })
  
}
    
## To be copied in the UI
# mod_correlacion_ui("correlacion_ui_1")
    
## To be copied in the server
# callModule(mod_correlacion_server, "correlacion_ui_1")
 
