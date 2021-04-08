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
  
  b.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                             column(width = 2,br(),actionButton(ns("runBoosting"), label = labelInput("ejecutar"), icon = icon("play")))),
                    hr(),
                    fluidRow(column(numericInput(ns("iter.boosting"), labelInput("numTree"), 20, width = "100%",min = 1), width = 6),
                             column(numericInput(ns("shrinkage.boosting"), labelInput("shrinkage"), 0.01, width = "100%",min = 0.001, step = 0.001), width=6)),
                    fluidRow(column(selectInput(inputId = ns("tipo.boosting"), label = labelInput("selectAlg"),selected = 1,
                                                choices =  c("gaussian", "laplace", "tdist")), width = 6)))
  
  b.code  <- list(h4(labelInput("codigo")), hr(),
                  conditionalPanel("input.BoxB == 'tabBModelo'",
                                   aceEditor(ns("fieldCodeBoosting"), mode = "r", theme = "monokai",
                                             value = "", height = "5vh", readOnly = F, autoComplete = "enabled"),ns = ns),
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
  
  tabs.b  <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(63, 95),
                         tabs.content = list(b.options, b.code))
  
  generate.b.panel <- tabPanel(title = labelInput("generatem"), value = "tabBModelo",
                               verbatimTextOutput(ns("txtBoosting")))
  
  plot.boosting.import <- tabPanel(title = labelInput("varImp"), value = "tabBImp",
                                   plotOutput(ns('plot.boosting.import'), height = "70vh"))
  
  prediction.b.panel <- tabPanel(title = labelInput("predm"), value = "tabBPred",
                                 DT::dataTableOutput(ns("boostingPrediTable")))
  
  disp.boosting.panel <- tabPanel(title = labelInput("dispersion"), value = "tabBDisp",
                                  plotOutput(ns('plot.boosting.disp'), height = "55vh"))
  
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
                                    tabs.b))
  
  tagList(
    pagina.boosting
  )
}
    
#' boosting Server Function
#'
#' @noRd 
mod_boosting_server <- function(input, output, session,updateData, updatePlot){
  ns <- session$ns
  
  # change model codes
  observeEvent(updateData$datos.aprendizaje,{
    deault_codigo_boosting()
  })
 
  # When the boosting model is generated
  observeEvent(input$runBoosting, {
    if (validate_data()){ # Si se tiene los datos entonces :
      boosting_full()
    }
  })
  
  # When user change the rule selector
  observeEvent(input$rules.b.n,{
    if(validate_data(print = FALSE)){
      mostrar.reglas.boosting(input$rules.b.n)
    }
  })
  
  # When the user changes the parameters
  observeEvent(c(input$iter.boosting, input$nu.boosting, input$tipo.boosting, input$shrinkage.boosting, input$maxdepth.boosting), {
    if (validate_data(print = FALSE)){
      deault_codigo_boosting()
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
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models(paste0("prediccion.boosting.",input$tipo.boosting), translate("bl"), variable.predecir)
    updateAceEditor(session, "fieldCodeBoostingDisp", value = codigo)
    
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
        remove_report_elem(paste0("modelo.b.",input$tipo.boosting))
        remove_report_elem(paste0("modelo.b.error.",input$tipo.boosting))
        remove_report_elem(paste0("modelo.b.imp.",input$tipo.boosting))
      }, {
        exe("prediccion.boosting.",input$tipo.boosting," <<- NULL")
        remove_report_elem(paste0("pred.b.",input$tipo.boosting))
        output$boostingPrediTable <- DT::renderDataTable(NULL)
      },{
        exe("indices.boosting.",input$tipo.boosting," <<- NULL")
        remove_report_elem(paste0("ind.b.",input$tipo.boosting))
      })
    }
  }
  
  # Shows the chart of importance
  plotear_boosting_imp <- function() {
    tryCatch({
      codigo <- input$fieldCodeBoostingPlotImport
      tipo <- input$tipo.boosting
      output$plot.boosting.import <- renderPlot(isolate(exe(codigo)))
      cod <- ifelse(codigo == "",boosting_importance_plot(paste0("modelo.boosting.",input$tipo.boosting)), codigo)
      insert_report(paste0("modelo.b.imp.",tipo), 
                    paste0("Importancia de las Variables (",tipo,")"),
                    cod)
    }, error = function(e) {
      clean_boosting(1)
    })
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_boosting <- function(){
    tryCatch({ # Se corren los codigo
      tipo <- input$tipo.boosting
      codigo <- input$fieldCodeBoostingDisp
      output$plot.boosting.disp <- renderPlot(exe(codigo))
      insert_report(paste0("disp.boosting.",tipo),paste0("Dispersi\u00F3n del Modelo BOOSTING (",tipo,")"), codigo)
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
      
      insert_report(paste0("modelo.b.",tipo), paste0("Generaci\u00F3n del Modelo BOOSTING (",tipo,")"),
                    cod.b.modelo, "\nmodelo.boosting.",tipo)
      
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
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
      insert_report(paste0("pred.b.",tipo),paste0("Predicci\u00F3n del Modelo BOOSTING (",tipo,")"),
                    cod.b.pred,"\nkt(head(tb_predic(real.val, prediccion.boosting.",input$tipo.boosting,")$x$data[,-1]))",interpretation = FALSE)
      
      plot_disp_boosting()
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(2)
      showNotification(paste0("Error (B-02) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_boosting_ind <- function() {
    if(exists(paste0("prediccion.boosting.",input$tipo.boosting))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.b.ind))
        isolate(tipo <- input$tipo.boosting)
        
        indices.boosting <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.boosting.",tipo))
        #eval(parse(text = paste0("indices.boosting.",tipo, "<<- indices.boosting")))
        
        insert_report(paste0("ind.b.",tipo), paste0("\u00CDndices Generales del Modelo (",tipo,")"),
                      cod.knn.ind, "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'] ,prediccion.boosting.",tipo,"))\n",
                      "indices.boosting <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.boosting.",tipo,")\n",
                      "IndicesM[['bl-",tipo,"']] <- indices.boosting")
        
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
 
