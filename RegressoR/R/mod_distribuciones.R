#' distribuciones UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_distribuciones_ui <- function(id){
  ns <- NS(id)
  
  
  # DISTRIBUTIONS PAGE ------------------------------------------------------------------------------------------------------
  
  distribution.options <- list(h4(labelInput("opciones")), hr(), colourpicker::colourInput(ns("col.dist"), labelInput("selcolor"), value = "#FF0000AA", allowTransparent = T))
  
  distribution.codes.fields <- list(h4(labelInput("codigo")), hr(),
                                    conditionalPanel(condition = "input.tabDyA == 'numericas'",
                                                     code_field(ns("run.dya.num"),ns("fieldCodeNum"), height = "7vh"),ns = ns),
                                    conditionalPanel(condition = "input.tabDyA == 'categoricas'",
                                                     code_field(ns("run.dya.cat"),ns("fieldCodeCat"), height = "7vh"),ns = ns))
  
  code.distributions <- list(h4(labelInput("codigo")), hr(),
                             tabBox(id = ns("tabCodeDyA"), width = NULL, title = labelInput("codedist"),
                                    tabPanel(title = labelInput("numericas"),
                                             aceEditor(ns("fieldFuncNum"),mode = "r",theme = "monokai",value = "",height = "285px",readOnly = T)),
                                    tabPanel(title = labelInput("categoricas"),
                                             aceEditor(ns("fieldFuncCat"),mode = "r",theme = "monokai",value = "",height = "165px",readOnly = T))))
  
  distribution.tabs <- tabsOptions(buttons = list(icon("gear"), icon("terminal"), icon("info"), icon("code")),
                                   widths = c(50, 100, 100, 100), heights = c(30, 35, 48, 80),
                                   tabs.content = list(distribution.options,
                                                       distribution.codes.fields,
                                                       list(DT::dataTableOutput(ns("mostrarAtipicos"))),
                                                       code.distributions))
  
  variable.selector.distribution <- tags$div(class = "multiple-select-var",
                                             conditionalPanel(condition = "input.tabDyA == 'numericas'",
                                                              selectInput(inputId = ns("sel.distribucion.num"),label = NULL,choices =  ""),ns = ns),
                                             conditionalPanel(condition = "input.tabDyA == 'categoricas'",
                                                              selectInput(inputId = ns("sel.distribucion.cat"),label = NULL,choices =  ""),ns = ns))
  
  numerical.distribution.results <- tabPanel(title = labelInput("numericas"), value = "numericas", 
                                             plotOutput(ns('plot.num'), height = "70vh"),
                                             actionButton(inputId=ns("distribucion_numerica"),label = "",style="display:none;"))
  
  categorical.distribution.results <- tabPanel(title = labelInput("categoricas"), value = "categoricas",plotOutput(ns('plot.cat'), height = "70vh"))
  
  page.distributions <- tabItem(tabName = "distribucion",
                                tabBox(id = ns("tabDyA"), width = NULL,
                                       title =  variable.selector.distribution,
                                       numerical.distribution.results,
                                       categorical.distribution.results,
                                       distribution.tabs))
  
  
  tagList(
    page.distributions
  )
}
    
#' distribuciones Server Function
#'
#' @noRd 
mod_distribuciones_server <- function(input, output, session, updateData, updatePlot, disp.ranges){
  ns <- session$ns
  
  # DISTRIBUTION PAGE -----------------------------------------------------------------------------------------------------
  
  # Some code fields that are not parameter-dependent
  updateAceEditor(session, "fieldFuncNum"  , extract_code("numerical_distribution"))
  updateAceEditor(session, "fieldFuncCat"  , extract_code("categorical_distribution"))
  
  # Show the graph of numerical distribution
  observeEvent(updateData$datos, {
    updateSelectInput(session, "sel.distribucion.num", choices = colnames_empty(var_numerical(updateData$datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames_empty(var_categorical(updateData$datos)))
    
    output$plot.num <- renderPlot({
      tryCatch({
        cod.dya.num <<- updatePlot$dya.num
        res <- isolate(exe(cod.dya.num))
        updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
        insert_report(paste0("dya.num.", input$sel.distribucion.num), "Distribuci\u00F3n y atipicidad", cod.dya.num)
        
        return(res)
      }, error = function(e) {
        if (ncol(var_numerical(datos)) == 0){
          error_variables( T)
        }else{
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })
  
  # Execute the code of the numerical chart
  observeEvent(input$run.dya.num, {
    updatePlot$dya.num <- input$fieldCodeNum
  })
  
  # Executes the code when parameters change
  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <<- def_code_num(data = "datos", color = input$col.dist,
                                        variable = input$sel.distribucion.num)
  })
  
  # Creates the atypical table
  observeEvent(c(input$distribucion_numerica), {
    output$mostrarAtipicos <- DT::renderDataTable({
      atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
      datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
      datos <- datos[order(datos[, input$sel.distribucion.num]), , drop = F]
      datatable(datos, options = list(dom = 't', scrollX = TRUE, scrollY = "28vh",pageLength = nrow(datos))) %>%
        formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
    })
  })
  
  # Show the graph of categorical distribution
  observeEvent(updateData$datos, {
    output$plot.cat <- renderPlot({
      tryCatch({
        cod.dya.cat <<- updatePlot$dya.cat
        res <- isolate(exe(cod.dya.cat))
        updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
        insert_report(paste0("dya.cat.", input$sel.distribucion.cat), "Distribuci\u00F3n", cod.dya.cat)
        return(res)
      }, error = function(e) {
        if (ncol(var_categorical(datos)) == 0){
          error_variables( T)
        }else{
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })
  
  # Change the code of the categorical graphic
  observeEvent(input$run.dya.cat, {
    updatePlot$dya.cat <- input$fieldCodeCat
  })
  
  # Executes the code when parameters change
  observeEvent(input$sel.distribucion.cat, {
    updatePlot$dya.cat <<- def_code_cat(variable = input$sel.distribucion.cat)
  })
 
}
    
## To be copied in the UI
# mod_distribuciones_ui("distribuciones_ui_1")
    
## To be copied in the server
# callModule(mod_distribuciones_server, "distribuciones_ui_1")
 
