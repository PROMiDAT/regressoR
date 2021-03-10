#' dispersion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dispersion_ui <- function(id){
  ns <- NS(id)
  
  # DISPERSION PAGE ---------------------------------------------------------------------------------------------------------
  
  tabs.dispersion  <-  tabsOptions(heights = c(30, 39),
                                   tabs.content = list(list(h4(labelInput("opciones")), hr(),
                                                            colourpicker::colourInput(ns("col.disp"), labelInput("selcolor"),
                                                                                      value = "#FF0000AA",allowTransparent = T)),
                                                       list(h4(labelInput("codigo")), hr(),
                                                            column(width = 12, code_field(ns("run.disp"), ns("fieldCodeDisp"), height = "7vh")))))
  
  #dispersion.code <- column(width = 12, code_field(runid = "run.disp", fieldid = "fieldCodeDisp", height = "8vh"))
  
  dispersion.data <- column(width = 4, DT::dataTableOutput(ns('mostrar.disp.zoom')), hr(), plotOutput(ns('plot.disp.zoom'), height = "41vh"))
  
  dispersion.options <- fluidRow(h4(style = "float:left;font-size: 20px;", labelInput("selvars")),
                                 tags$div(class="multiple-select-var",style = "width:60%;",
                                          selectizeInput(ns("select.var"), NULL, multiple = T, choices = c(""),
                                                         options = list(maxItems = 3))))
  
  dispersion.plot <- tabPanel(title = labelInput("dispersion"), value = "tabDisp",
                              fluidRow(column(width = 8, plotOutput(ns('plot.disp'), height = "65vh",
                                                                    brush = brushOpts(id = ns("zoom.disp"), resetOnNew = TRUE))),
                                       dispersion.data))
  
  page.dispersion<- tabItem(tabName = "dispersion",
                            tabBox(id = ns("BoxDisp"), width = NULL, title = dispersion.options,
                                   dispersion.plot,
                                   tabs.dispersion))
  
  
  tagList(
    page.dispersion
  )
}
    
#' dispersion Server Function
#'
#' @noRd 
mod_dispersion_server <- function(input, output, session, updateData, updatePlot, disp.ranges){
  ns <- session$ns
  
  # DISPERSION PAGE -------------------------------------------------------------------------------------------------------
  
  # Show the scatter plot
  observeEvent(updateData$datos, {
    updateSelectizeInput(session, "select.var", choices = colnames_empty(var_numerical(updateData$datos)))
    
    output$plot.disp <- renderPlot({
      tryCatch({
        cod.disp <<- updatePlot$disp
        updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
        if(!is.null(cod.disp) && cod.disp != "") {
          insert_report(paste0("dispersion.", paste(input$select.var, collapse = ".")), "Dispersi\u00F3n", cod.disp)
        }
        return(isolate(exe(cod.disp)))
      }, error = function(e) {
        if(ncol(var_numerical(datos)) <= 1){
          error_variables( T)
        } else {
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })
  
  # Show the zoom graph
  output$plot.disp.zoom <- renderPlot({
    tryCatch({
      cod.disp <<- updatePlot$disp
      res <- isolate(exe(cod.disp))
      res <- res + coord_cartesian(xlim = disp.ranges$x, ylim = disp.ranges$y, expand = FALSE)
      return(res)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Show the table with the dispersion values
  output$mostrar.disp.zoom <- DT::renderDataTable({
    tryCatch({
      return(brushedPoints(datos[, input$select.var], input$zoom.disp))
    }, error = function(e) {
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh", pageLength = nrow(datos)))
  
  # When a zoom area is selected
  observe({
    brush <- input$zoom.disp
    if (!is.null(brush)) {
      disp.ranges$x <- c(brush$xmin, brush$xmax)
      disp.ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      disp.ranges$x <- NULL
      disp.ranges$y <- NULL
    }
  })
  
  # Change the graphic code
  observeEvent(input$run.disp, {
    updatePlot$disp <- input$fieldCodeDisp
  })
  
  # Executes the code when parameters change
  observeEvent(c(input$select.var, input$col.disp), {
    if (length(input$select.var) < 2) {
      updatePlot$disp <- ""
    } else {
      updatePlot$disp <<- default_disp(data = "datos", vars = input$select.var, color = input$col.disp)
    }
  })
  
 
}
    
## To be copied in the UI
# mod_dispersion_ui("dispersion_ui_1")
    
## To be copied in the server
# callModule(mod_dispersion_server, "dispersion_ui_1")
 
