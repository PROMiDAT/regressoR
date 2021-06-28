#' boosting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boosting_ui <- function(id){
  
  ns <- NS(id)
  
  b.options <- list(options.run(ns("runBoosting")), tags$hr(style = "margin-top: 0px;"),
                    fluidRow(column(numericInput(ns("iter.boosting"), labelInput("numTree"), 20, width = "100%",min = 1), width = 6),
                             column(numericInput(ns("shrinkage.boosting"), labelInput("shrinkage"), 0.1, width = "100%",min = 0.01, step = 0.01), width=6)),
                    fluidRow(column(selectInput(inputId = ns("tipo.boosting"), label = labelInput("selectAlg"),selected = "gaussian",
                                                choices =  c("gaussian", "laplace", "tdist")), width = 6)))
  
  b.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                        aceEditor(ns("fieldCodeBoosting"), mode = "r", theme = "monokai",
                                  value = "", height = "5vh", readOnly = F, autoComplete = "enabled"))
  
  
  b.code  <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                  conditionalPanel("input.BoxB == 'tabBImp'",
                                   aceEditor(ns("fieldCodeBoostingPlotImport"), mode = "r", theme = "monokai",
                                             value = "", height = "10vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxB == 'tabBPred'",
                                   aceEditor(ns("fieldCodeBoostingPred"), mode = "r", theme = "monokai",
                                             value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxB == 'tabBDisp'",
                                   aceEditor(ns("fieldCodeBoostingDisp"), mode = "r", theme = "monokai",
                                             value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxB == 'tabBIndex'",
                                   aceEditor(ns("fieldCodeBoostingIG"), mode = "r", theme = "monokai",
                                             value = "", height = "22vh", readOnly = F, autoComplete = "enabled"),ns = ns))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(80,95),
                                       tabs.content = list(b.options,b.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(95),
                                         tabs.content = list(b.code))
  
  generate.b.panel <- tabPanel(title = labelInput("generatem"), value = "tabBModelo",
                               verbatimTextOutput(ns("txtBoosting")))
  
  plot.boosting.import <- tabPanel(title = labelInput("varImp"), value = "tabBImp",
                                   plotOutput(ns('plot.boosting.import'), height = "70vh"))
  
  prediction.b.panel <- tabPanel(title = labelInput("predm"), value = "tabBPred",
                                 DT::dataTableOutput(ns("boostingPrediTable")))
  
  disp.boosting.panel <- tabPanel(title = labelInput("dispersion"), value = "tabBDisp",
                                  echarts4rOutput(ns('plot.boosting.disp'), height = "75vh"))
  
  general.index.b.panel <- tabPanel(title = labelInput("indices"),value = "tabBIndex",
                                    br(),
                                    fluidRow(tableOutput(ns('indexdfb'))),
                                    br(),
                                    fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                    br(),
                                    fluidRow(tableOutput(ns('indexdfb2'))))
  
  pagina.boosting <- tabItem(tabName = "boosting",
                             tabBox(id = ns("BoxB"), width = NULL, height ="80%",
                                    generate.b.panel,
                                    plot.boosting.import,
                                    prediction.b.panel,
                                    disp.boosting.panel,
                                    general.index.b.panel,
                                    conditionalPanel("input.BoxB == 'tabBModelo'",tabs.options.generate,ns = ns),
                                    conditionalPanel("input.BoxB != 'tabBModelo'",tabs.options.Nogenerate,ns = ns)))
  
  tagList(
    pagina.boosting
  )
}
    
#' boosting Server Function
#'
#' @noRd 
mod_boosting_server <- function(input, output, session,updateData, updatePlot){
  ns <- session$ns
  
  return.boosting.default.values <- function(){
    updateSelectInput(session,inputId = "tipo.boosting", selected = "gaussian")
    updateNumericInput(session, inputId = "iter.boosting", value = 20)
    updateNumericInput(session, inputId = "shrinkage.boosting", value = 0.1)
    
    
    output$txtBoosting <- renderText(NULL)
    output$plot.boosting.import <- renderPlot(NULL)
    output$boostingPrediTable <- DT::renderDataTable(NULL)
    output$plot.boosting.disp <- renderPlot(NULL)
    output$indexdfb <- render_index_table(NULL)
    output$indexdfb2 <- render_index_table(NULL)
  }
  
  
  observeEvent(updateData$idioma,{
    execute_boosting_ind()
    plot_disp_boosting()
  })

  observeEvent(updateData$datos.aprendizaje,{
    return.boosting.default.values()
  })
 
  # When the boosting model is generated
  observeEvent(input$runBoosting, {
    if (validate_data()){ # Si se tiene los datos entonces :
      # change model codes
      deault_codigo_boosting()
      boosting_full()
    }
  })

  
  # Upgrade code fields to default version
  deault_codigo_boosting <- function() {
    # Se acualiza el codigo del modelo
    codigo <- boosting_model(variable.pred = variable.predecir,
                             model.var = paste0("modelo.boosting.",input$tipo.boosting),
                             n.trees = input$iter.boosting,
                             distribution = input$tipo.boosting,
                             shrinkage = input$shrinkage.boosting)
    
    updateAceEditor(session, "fieldCodeBoosting", value = codigo)
    cod.b.modelo <<- codigo
    
    # Se genera el codigo de la prediccion
    codigo <- boosting_prediction(variable.pred = variable.predecir, 
                                  model.var = paste0("modelo.boosting.",input$tipo.boosting),
                                  pred.var = paste0("prediccion.boosting.",input$tipo.boosting),
                                  n.trees = input$iter.boosting)
    
    updateAceEditor(session, "fieldCodeBoostingPred", value = codigo)
    cod.b.pred <<- codigo

    
    # Cambia el codigo del grafico de importancia
    updateAceEditor(session, "fieldCodeBoostingPlotImport", value = boosting_importance_plot(paste0("modelo.boosting.",input$tipo.boosting)))
    
    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeBoostingIG", value = codigo)
    cod.b.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_boosting <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        exe("modelo.boosting.",input$tipo.boosting," <<- NULL")
        output$txtBoosting <- renderPrint(invisible(""))
        output$plot.boosting.import <- renderPlot(NULL)
      }, {
        exe("prediccion.boosting.",input$tipo.boosting," <<- NULL")
        output$boostingPrediTable <- DT::renderDataTable(NULL)
      },{
        exe("indices.boosting.",input$tipo.boosting," <<- NULL")
      })
    }
  }
  
  # Shows the chart of importance
  plotear_boosting_imp <- function() {
    tryCatch({
      output$plot.boosting.import <- renderPlot(isolate(exe(boosting_importance_plot(paste0("modelo.boosting.",input$tipo.boosting)))))
    }, error = function(e) {
      clean_boosting(1)
    })
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_boosting <- function(){
    tryCatch({ # Se corren los codigo
      titulos <- c(
        tr("predvsreal", updateData$idioma),
        tr("realValue", updateData$idioma),
        tr("pred", updateData$idioma)
      )
      
      output$plot.boosting.disp <- renderEcharts4r(plot_real_prediction(datos.prueba[variable.predecir],
                                                                  exe(paste0("prediccion.boosting.",input$tipo.boosting)),
                                                                  translate("bl"),titulos))
      
      codigo <- disp_models(paste0("prediccion.boosting.",input$tipo.boosting), translate("bl"), variable.predecir)
      updateAceEditor(session, "fieldCodeBoostingDisp", value = codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(2)
      showNotification(paste0("Error (B-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  boosting_full <- function() {
    if(!is.null(calibrate_boosting(datos.aprendizaje))){
      execute_boosting()
      execute_boosting_pred()
      execute_boosting_ind()
    }else{
      showNotification(translate("ErrorBsize"), duration = 15, type = "error")
    }
  }
  
  # Generates the model
  execute_boosting <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.b.modelo))
      isolate(tipo <- input$tipo.boosting)
      output$txtBoosting <- renderPrint(exe("print(summary(modelo.boosting.",tipo,",plotit = FALSE))"))
      
      plotear_boosting_imp()
      
      #nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(1)
      showNotification(paste0("Error (B-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_boosting_pred <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.b.pred))
      isolate(tipo <- input$tipo.boosting)
      
      # Cambia la tabla con la prediccion de boosting
      output$boostingPrediTable <- DT::renderDataTable(tb_predic(real.val, exe("prediccion.boosting.",tipo)),server = FALSE)
      
      plot_disp_boosting()
      #nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(2)
      showNotification(paste0("Error (B-02) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_boosting_ind <- function() {
    var.prediction.name <- paste0("prediccion.boosting.",input$tipo.boosting)
    if(exists(var.prediction.name)){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.b.ind))
        isolate(tipo <- input$tipo.boosting)
        
        indices.boosting <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.boosting.",tipo))
        
        df <- as.data.frame(indices.boosting)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfb <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfb2 <- render_index_table(df2)
        
        #nombres.modelos <<- c(nombres.modelos, paste0("indices.boosting.",tipo))
        updateData$IndicesM[[paste0("bl-",tipo)]] <<- indices.boosting
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_boosting(3)
        showNotification(paste0("Error (B-03) : ", e), duration = 15, type = "error")
      })
    }
  }
}
    
## To be copied in the UI
# mod_boosting_ui("boosting_ui_1")
    
## To be copied in the server
# callModule(mod_boosting_server, "boosting_ui_1")
 
