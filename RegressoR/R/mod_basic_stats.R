#' basic_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_basic_stats_ui <- function(id){
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
  
  # NORMALITY TEST PAGE -----------------------------------------------------------------------------------------------------
  
  num.normal.plot.panel <- tabPanel(title = labelInput("plotnormal"), value = "tabNormalPlot", plotOutput(ns('plot.normal'), height = "65vh"))
  
  cat.normal.plot.panel <- tabPanel(title = labelInput("normalidad"), value = "tabNormalCalc", DT::dataTableOutput(ns('calculo.normal')))
  
  boton.colores <- list(h4(labelInput("opciones")), hr(),
                        colourpicker::colourInput(ns("col.normal"), labelInput("selcolor"),value = "#00FF22AA", allowTransparent = T))
  
  normality.code <- list(h4(labelInput("codigo")), hr(),
                         conditionalPanel("input.BoxNormal == 'tabNormalCalc'",
                                          code_field("run.calc.normal", "fieldCalcNormal", height = "20vh"),ns = ns),
                         conditionalPanel("input.BoxNormal == 'tabNormalPlot'",
                                          code_field("run.normal", "fieldCodeNormal", height = "25vh"),ns = ns))
  
  tabs.normal <- tabsOptions(heights = c(33, 63), tabs.content = list(boton.colores, normality.code))
  
  normal.options <-  tags$div(class = "multiple-select-var", selectInput(inputId = ns("sel.normal"), label = NULL, choices =  ""))
  
  page.test.normality <- tabItem(tabName = "normalidad",
                                 tabBox(id = ns("BoxNormal"),
                                        width = 12, title = normal.options,
                                        num.normal.plot.panel,
                                        cat.normal.plot.panel,
                                        tabs.normal))
  
  # DISPERSION PAGE ---------------------------------------------------------------------------------------------------------
  
  tabs.dispersion  <-  tabsOptions(heights = c(30, 39),
                                   tabs.content = list(list(h4(labelInput("opciones")), hr(),
                                                            colourpicker::colourInput("col.disp", labelInput("selcolor"),
                                                                                      value = "#FF0000AA",allowTransparent = T)),
                                                       list(h4(labelInput("codigo")), hr(),
                                                            column(width = 12, code_field("run.disp", "fieldCodeDisp", height = "7vh")))))
  
  #dispersion.code <- column(width = 12, code_field(runid = "run.disp", fieldid = "fieldCodeDisp", height = "8vh"))
  
  dispersion.data <- column(width = 4, DT::dataTableOutput('mostrar.disp.zoom'), hr(), plotOutput('plot.disp.zoom', height = "41vh"))
  
  dispersion.options <- fluidRow(h4(style = "float:left;font-size: 20px;", labelInput("selvars")),
                                 tags$div(class="multiple-select-var",style = "width:60%;",
                                          selectizeInput("select.var", NULL, multiple = T, choices = c(""),
                                                         options = list(maxItems = 3))))
  
  dispersion.plot <- tabPanel(title = labelInput("dispersion"), value = "tabDisp",
                              fluidRow(column(width = 8, plotOutput('plot.disp', height = "65vh",
                                                                    brush = brushOpts(id = "zoom.disp", resetOnNew = TRUE))),
                                       dispersion.data))
  
  page.dispersion<- tabItem(tabName = "dispersion",
                            tabBox(id = "BoxDisp", width = NULL, title = dispersion.options,
                                   dispersion.plot,
                                   tabs.dispersion))
  
  # DISTRIBUTIONS PAGE ------------------------------------------------------------------------------------------------------
  
  distribution.options <- list(h4(labelInput("opciones")), hr(), colourpicker::colourInput("col.dist", labelInput("selcolor"), value = "#FF0000AA", allowTransparent = T))
  
  distribution.codes.fields <- list(h4(labelInput("codigo")), hr(),
                                    conditionalPanel(condition = "input.tabDyA == 'numericas'",
                                                     code_field("run.dya.num","fieldCodeNum", height = "7vh")),
                                    conditionalPanel(condition = "input.tabDyA == 'categoricas'",
                                                     code_field("run.dya.cat","fieldCodeCat", height = "7vh")))
  
  code.distributions <- list(h4(labelInput("codigo")), hr(),
                             tabBox(id = "tabCodeDyA", width = NULL, title = labelInput("codedist"),
                                    tabPanel(title = labelInput("numericas"),
                                             aceEditor("fieldFuncNum",mode = "r",theme = "monokai",value = "",height = "285px",readOnly = T)),
                                    tabPanel(title = labelInput("categoricas"),
                                             aceEditor("fieldFuncCat",mode = "r",theme = "monokai",value = "",height = "165px",readOnly = T))))
  
  distribution.tabs <- tabsOptions(buttons = list(icon("gear"), icon("terminal"), icon("info"), icon("code")),
                                   widths = c(50, 100, 100, 100), heights = c(30, 35, 48, 80),
                                   tabs.content = list(distribution.options,
                                                       distribution.codes.fields,
                                                       list(DT::dataTableOutput("mostrarAtipicos")),
                                                       code.distributions))
  
  variable.selector.distribution <- tags$div(class = "multiple-select-var",
                                             conditionalPanel(condition = "input.tabDyA == 'numericas'",
                                                              selectInput(inputId = "sel.distribucion.num",label = NULL,choices =  "")),
                                             conditionalPanel(condition = "input.tabDyA == 'categoricas'",
                                                              selectInput(inputId = "sel.distribucion.cat",label = NULL,choices =  "")))
  
  numerical.distribution.results <- tabPanel(title = labelInput("numericas"), value = "numericas", 
                                             plotOutput('plot.num', height = "70vh"),
                                             actionButton(inputId="distribucion_numerica",label = "",style="display:none;"))
  
  categorical.distribution.results <- tabPanel(title = labelInput("categoricas"), value = "categoricas",plotOutput('plot.cat', height = "70vh"))
  
  page.distributions <- tabItem(tabName = "distribucion",
                                tabBox(id = "tabDyA", width = NULL,
                                       title =  variable.selector.distribution,
                                       numerical.distribution.results,
                                       categorical.distribution.results,
                                       distribution.tabs))
  
  # CORRELATIONS PAGE -------------------------------------------------------------------------------------------------------
  
  cor.options <-list(h4(labelInput("opciones")), hr(),
                     selectInput(inputId = "cor.metodo", label = labelInput("selmetodo"),
                                 choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                     selectInput(inputId = "cor.tipo", label = labelInput("seltipo"), choices =  c("lower", "upper", "full")))
  
  cor.code <- list(h4(labelInput("codigo")), hr(),
                   aceEditor("fieldModelCor", height = "6vh", mode = "r", theme = "monokai", value = "", readOnly = T),
                   code_field("run.code.cor","fieldCodeCor", height = "7vh"))
  
  cor.tabs <- tabsOptions(heights = c(48, 63),
                          tabs.content = list(cor.options, cor.code))
  
  correlation.plot <- tabPanel(title = labelInput("correlacion"), value = "correlacion", plotOutput('plot.cor', height = "70vh"))
  
  results.table.correlations <- tabPanel(title = labelInput("resultados"), value = "cor.salida", verbatimTextOutput("txtcor"))
  
  page.correlations <- tabItem(tabName = "correlacion",
                               tabBox(id = "tabCor", width = NULL,
                                      correlation.plot,
                                      results.table.correlations,
                                      cor.tabs))
  
  # PREDICTIVE POWER PAGE ---------------------------------------------------------------------------------------------------
  
  code.power.num <- list(h4(labelInput("codigo")), hr(),
                         code_field(runid = "run.code.poder.num", fieldid = "fieldCodePoderNum", height = "16vh"))
  
  
  tabs.power.num <- tabsOptions(buttons = list(icon("terminal")), widths = 100, heights = 55,
                                tabs.content = list(code.power.num))
  
  power.plot.pairs <- tabPanel(title = labelInput('pares'), value = "predpares",
                               plotOutput('plot.pairs.poder', height = "55vh"))
  
  pagina.poder <- tabItem(tabName = "poderPred",
                          tabBox(id = "BoxPodPred", width = NULL,
                                 power.plot.pairs,
                                 tabs.power.num))
  
  # RL PAGE -----------------------------------------------------------------------------------------------------------------
  
  rl.code  <- list(fluidRow(column(width = 9,h4(labelInput("codigo"))),
                            column(width = 2,br(),actionButton("runRl", label = labelInput("ejecutar"), icon = icon("play")))),
                   hr(),
                   conditionalPanel("input.BoxRl == 'tabRlModelo'",
                                    aceEditor("fieldCodeRl", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRl == 'tabRlCoef'",
                                    aceEditor("fieldCodeRlCoef", mode = "r", theme = "monokai",
                                              value = "", height = "10vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRl == 'tabRlPred'",
                                    aceEditor("fieldCodeRlPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRl == 'tabRlDisp'",
                                    aceEditor("fieldCodeRlDisp", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRl == 'tabRlIndex'",
                                    aceEditor("fieldCodeRlIG", mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled")))
  
  tabs.rl  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(95),
                          tabs.content = list(rl.code))
  
  generate.rl.panel <- tabPanel(title = labelInput("generatem"),value = "tabRlModelo",
                                verbatimTextOutput("txtRl"))
  
  coefficients.rl.panel <- tabPanel(title = labelInput("coeff"), value = "tabRlCoef",
                                    DT::dataTableOutput("rlCoefTable"))
  
  prediccion.rl.panel <- tabPanel(title = labelInput("predm"), value = "tabRlPred",
                                  DT::dataTableOutput("rlPrediTable"))
  
  disp.rl.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRlDisp",
                            plotOutput('plot.rl.disp', height = "55vh"))
  
  rl.general.index.panel <- tabPanel(title = labelInput("indices"), value = "tabRlIndex",
                                     br(),
                                     fluidRow(tableOutput('indexdfrl')),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(tableOutput('indexdfrl2')))
  
  
  page.rl <- tabItem(tabName = "rl",
                     tabBox(id = "BoxRl", width = NULL, height ="80%",
                            generate.rl.panel,
                            coefficients.rl.panel,
                            prediccion.rl.panel,
                            disp.rl.panel,
                            rl.general.index.panel,
                            tabs.rl))
  
  tagList(
 
  )
}
    
#' basic_stats Server Function
#'
#' @noRd 
mod_basic_stats_server <- function(input, output, session, updatePlot, disp.ranges){
  ns <- session$ns
 
  # SUMMARY PAGE ----------------------------------------------------------------------------------------------------------
  
  # Change the table with the summary on the summary page
  output$resumen.completo <- DT::renderDataTable({ insert_report("resumen","Resumen Num\u00E9rico", "summary(datos)")
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
  
  # NORMALITY TEST PAGE ---------------------------------------------------------------------------------------------------
  
  # Show the graph of the normality test page
  observeEvent(c(input$loadButton, input$transButton), {
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
  observeEvent(c(input$loadButton, input$transButton), {
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
  
  # DISPERSION PAGE -------------------------------------------------------------------------------------------------------
  
  # Show the scatter plot
  observeEvent(c(input$loadButton, input$transButton), {
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
  
  # DISTRIBUTION PAGE -----------------------------------------------------------------------------------------------------
  
  # Show the graph of numerical distribution
  observeEvent(c(input$loadButton, input$transButton), {
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
  observeEvent(c(input$loadButton, input$transButton), {
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
  
  # CORRELATION PAGE ------------------------------------------------------------------------------------------------------
  
  # Show the correlation graph
  observeEvent(c(input$loadButton, input$transButton, input$fieldModelCor), {
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
  
  # PREDICTIVE POWER PAGE -------------------------------------------------------------------------------------------------
  
  # Show the graph of numerical predictive power
  observeEvent(input$segmentButton,{
    output$plot.pairs.poder <- renderPlot({
      tryCatch({
        cod.poder.num <<- updatePlot$poder.num
        updateAceEditor(session, "fieldCodePoderNum", value = cod.poder.num)
        if (ncol(var_numerical(datos)) >= 2) {
          if(ncol(var_numerical(datos)) <= 25){
            res <- isolate(exe(cod.poder.num))
            insert_report("poder.num","Poder Predictivo Variables Num\u00E9ricas", cod.poder.num)
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
  observeEvent(input$segmentButton,{
    updatePlot$poder.num <- pairs_power()
  }, priority = 3)
}
    
## To be copied in the UI
# mod_basic_stats_ui("basic_stats_ui_1")
    
## To be copied in the server
# callModule(mod_basic_stats_server, "basic_stats_ui_1")
 
