#' normal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_normal_ui <- function(id){
  ns <- NS(id)
  
  # NORMALITY TEST PAGE -----------------------------------------------------------------------------------------------------
  
  num.normal.plot.panel <- tabPanel(title = labelInput("plotnormal"), value = "tabNormalPlot", plotOutput(ns('plot.normal'), height = "65vh"))
  
  cat.normal.plot.panel <- tabPanel(title = labelInput("normalidad"), value = "tabNormalCalc", DT::dataTableOutput(ns('calculo.normal')))
  
  boton.colores <- list(h4(labelInput("opciones")), hr(),
                        colourpicker::colourInput(ns("col.normal"), labelInput("selcolor"),value = "#00FF22AA", allowTransparent = T))
  
  normality.code <- list(h4(labelInput("codigo")), hr(),
                         conditionalPanel("input.BoxNormal == 'tabNormalCalc'",
                                          code_field(ns("run.calc.normal"), ns("fieldCalcNormal"), height = "20vh"),ns = ns),
                         conditionalPanel("input.BoxNormal == 'tabNormalPlot'",
                                          code_field(ns("run.normal"), ns("fieldCodeNormal"), height = "25vh"),ns = ns))
  
  tabs.normal <- tabsOptions(heights = c(33, 63), tabs.content = list(boton.colores, normality.code))
  
  normal.options <-  tags$div(class = "multiple-select-var", selectInput(inputId = ns("sel.normal"), label = NULL, choices =  ""))
  
  page.test.normality <- tabItem(tabName = "normalidad",
                                 tabBox(id = ns("BoxNormal"),
                                        width = 12, title = normal.options,
                                        num.normal.plot.panel,
                                        cat.normal.plot.panel,
                                        tabs.normal))
  
  tagList(
    page.test.normality
  )
}
    
#' normal Server Function
#'
#' @noRd 
mod_normal_server <- function(input, output, session, updateData,updatePlot, disp.ranges){
  ns <- session$ns
 
  
  # NORMALITY TEST PAGE ---------------------------------------------------------------------------------------------------
  
  # Show the graph of the normality test page
  observeEvent(updateData$datos, {
    updateSelectizeInput(session, "sel.normal", choices = colnames_empty(var_numerical(updateData$datos)))
    
    output$plot.normal <- renderPlot({
      tryCatch({
        cod.normal <<- updatePlot$normal
        res <- isolate(exe(cod.normal))
        updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
        insert_report(paste0("normalidad.", input$sel.normal), "Test de Normalidad", cod.normal)
        return(res)
      }, error = function(e){
        if(ncol(var_numerical(datos)) <= 0){
          error_variables( T)
        } else {
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })
  
  # Change the code in the code field
  observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })
  
  # Executes the code when parameters change
  observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- normal_default(data = "datos", vars = input$sel.normal, color = input$col.normal, translate("curvanormal"))
  })
  
  # Show the comparative table of the normality test page
  observeEvent(updateData$datos, {
    output$calculo.normal <- DT::renderDT({
      tryCatch({
        codigo <- updatePlot$calc.normal
        res    <- isolate(exe(codigo))
        updateAceEditor(session, "fieldCalcNormal", value = codigo)
        fisher    <- translate("fisher")
        asimetria <- translate("asimetria")
        sketch = htmltools::withTags(table(tags$thead(tags$tr(tags$th(), tags$th(fisher), tags$th(asimetria)))))
        DT::datatable(res, selection = 'none', container = sketch, options = list(dom = 'frtip', scrollY = "40vh"))
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })
  
  # Run the comparison table
  observeEvent(input$run.calc.normal, {
    updatePlot$calc.normal <- input$fieldCalcNormal
  })
  
  
}
    
## To be copied in the UI
# mod_normal_ui("normal_ui_1")
    
## To be copied in the server
# callModule(mod_normal_server, "normal_ui_1")
 
