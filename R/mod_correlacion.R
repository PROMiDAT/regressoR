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
  
  correlation.plot <- tabPanel(title = labelInput("correlacion"), value = "correlacion",
                               echarts4rOutput(ns('plot_cor'), height = "70vh"))
  
  results.table.correlations <- tabPanel(title = labelInput("resultados"), value = "cor.salida",
                                         div(style = "height: 60vh;overflow-y: scroll;",
                                             withLoader(verbatimTextOutput(ns("txt_cor")),
                                                        type = "html", loader = "loader4")))
  
  corr.plot.opc <- list(options.run(ns("run_cor")), tags$hr(style = "margin-top: 0px;"),
                          colourpicker::colourInput(ns("col_min"), labelInput("selcolor"), "#FF5733",allowTransparent = T),
                          colourpicker::colourInput(ns("col_med"), labelInput("selcolor"), "#F8F5F5",allowTransparent = T),
                          colourpicker::colourInput(ns("col_max"), labelInput("selcolor"), "#2E86C1",allowTransparent = T))
  
  corr.plot.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                           codigo.monokai(ns("fieldCodeCor"),  height = "24vh"))
  
  corr.table.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                          codigo.monokai(ns("fieldCodeCor2"),  height = "10vh"))
  
  tabs.plot <- tabsOptions(buttons = list(icon("gear"), icon("code")),heights = c(70, 50),
                           tabs.content = list(corr.plot.opc,corr.plot.code))
  
  tabs.table <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                            tabs.content = list(corr.table.code))

  
  page.correlations <- tabItem(tabName = "correlacion",
                               tabBox(id = ns("tabCor"), width = NULL,
                                      correlation.plot,
                                      results.table.correlations,
                                      conditionalPanel("input.tabCor == 'correlacion'",tabs.plot,ns = ns),
                                      conditionalPanel("input.tabCor != 'correlacion'",tabs.table,ns = ns)
                                      ))
  
  tagList(
    page.correlations
  )
}
    
#' correlacion Server Function
#'
#' @noRd 
mod_correlacion_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  #' Gráfico de Correlaciones
  output$plot_cor <- renderEcharts4r({
    input$run_cor
    datos <- var_numerical(updateData$datos)
    col_min <- isolate(input$col_min)
    col_med <- isolate(input$col_med)
    col_max <- isolate(input$col_max)
    colores <- list(list(0, col_min), list(0.5, col_med), list(1, col_max))
    
    tryCatch({
      cod <- code.cor(col_min, col_med, col_max)
      updateAceEditor(session, "fieldCodeCor", value = cod)
      
      datos.plot <- round(cor(datos), 3)
      datos.plot %>% e_charts() %>% 
        e_correlations(
          order = "hclust", label = list(show = T),
          inRange = list(color = c(col_min, col_med, col_max)),
          itemStyle = list(borderWidth = 2, borderColor = "#fff")
        ) %>% e_datazoom(show = F) %>% e_show_loading() %>% e_tooltip(
          formatter = htmlwidgets::JS(paste0(
            "function(params) {\n",
            "  return(params.value[1] + ' ~ ' + params.value[0] + ': ' + parseFloat(params.value[2]).toFixed(3))\n", 
            "}"))
        )
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Resultados numéricos de Correlaciones
  output$txt_cor <- renderPrint({
    cod <- "cor(var_numerical(datos))"
    updateAceEditor(session, "fieldCodeCor2", value = cod)
    print(cor(var_numerical(updateData$datos)))
  })
}
    
## To be copied in the UI
# mod_correlacion_ui("correlacion_ui_1")
    
## To be copied in the server
# callModule(mod_correlacion_server, "correlacion_ui_1")
 
