#' random_forests UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_random_forests_ui <- function(id){
  
  ns <- NS(id)
  
  rf.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,br(),actionButton(ns("runRf"), label = labelInput("ejecutar"), icon = icon("play")))),
                     hr(),
                     conditionalPanel("input.BoxRf != 'tabRfRules'",
                                      fluidRow(column(numericInput(ns("ntree.rf"), labelInput("numTree"), 20, width = "100%", min = 0), width = 6),
                                               column(numericInput(ns("mtry.rf"),labelInput("numVars"),1, width = "100%", min = 1), width=6)), ns = ns),
                     conditionalPanel("input.BoxRf == 'tabRfRules'",
                                      numericInput(ns("rules.rf.n"),labelInput("ruleNumTree"),1, width = "100%", min = 1),ns = ns))
  
  rf.code  <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxRf == 'tabRfModelo'",
                                    aceEditor(ns("fieldCodeRf"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRf == 'tabRfImp'",
                                    aceEditor(ns("fieldCodeRfPlot"), mode = "r", theme = "monokai",
                                              value = "", height = "28vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRf == 'tabRfPred'",
                                    aceEditor(ns("fieldCodeRfPred"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRf == 'tabRfDisp'",
                                    aceEditor(ns("fieldCodeRfDisp"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRf == 'tabRfIndex'",
                                    aceEditor(ns("fieldCodeRfIG"), mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRf == 'tabRfRules'",
                                    aceEditor(ns("fieldCodeRfRules"), mode = "r", theme = "monokai",
                                              value = "", height = "4vh", readOnly = F, autoComplete = "enabled"),ns = ns))
  
  tabs.rf  <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(65, 95),
                          tabs.content = list(rf.options, rf.code))
  
  generate.rf.panel <- tabPanel(title = labelInput("generatem"),value = "tabRfModelo",
                                verbatimTextOutput(ns("txtRf")))
  
  plot.rf <- tabPanel(title = labelInput("varImp"), value = "tabRfImp",
                      plotOutput(ns('plot.rf'), height = "70vh"))
  
  prediction.rf.panel <- tabPanel(title = labelInput("predm"), value = "tabRfPred",
                                  DT::dataTableOutput(ns("rfPrediTable")))
  
  disp.rf.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRfDisp",
                            plotOutput(ns('plot.rf.disp'), height = "55vh"))
  
  general.index.rf.panel <- tabPanel(title = labelInput("indices"), value = "tabRfIndex",
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfrf'))),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfrf2'))))
  
  rf.rules.panel <- tabPanel(title = labelInput("reglas"), value = "tabRfRules",
                             verbatimTextOutput(ns("rulesRf")))
  
  page.rf <- tabItem(tabName = "rf",
                     tabBox(id = ns("BoxRf"), width = NULL, height ="80%",
                            generate.rf.panel,
                            plot.rf,
                            prediction.rf.panel,
                            disp.rf.panel,
                            general.index.rf.panel,
                            rf.rules.panel,
                            tabs.rf))
  
  
  tagList(
    page.rf
  )
}
    
#' random_forests Server Function
#'
#' @noRd 
mod_random_forests_server <- function(input, output, session,updateData, updatePlot){
  ns <- session$ns
  
  return.rf.default.values <- function(){
    updateNumericInput(session = session, inputId = "ntree.rf", value = 20)
    
    rf.args.default <<- TRUE
    
    output$txtRf <- renderText(NULL)
    output$plot.rf <- renderPlot(NULL)
    output$rfPrediTable <- DT::renderDataTable(NULL)
    output$plot.rf.disp <- renderPlot(NULL)
    output$indexdfrf <- render_index_table(NULL)
    output$indexdfrf2 <- render_index_table(NULL)
    output$rulesRf <- renderText(NULL)
  }
  
  observeEvent(updateData$idioma,{
    execute_rf_ind()
  })
  
  # change model codes
  observeEvent(updateData$datos.aprendizaje,{
    return.rf.default.values()
  })
 
  
  # When the rf model is generated
  observeEvent(input$runRf, {
    if (validate_data()) { # Si se tiene los datos entonces :
      deafult_codigo_rf()
      rf_full()
    }
  })
  
  # When the user changes the parameters
  # observeEvent(c(input$ntree.rf,input$mtry.rf), {
  #   if (validate_data(print = FALSE) & rf.stop.excu) {
  #     deafult_codigo_rf()
  #   }else{
  #     rf.stop.excu <<- TRUE
  #   }
  # })
  
  # When user change the rule selector
  observeEvent(input$rules.rf.n,{
    if(validate_data(print = FALSE)){
      show_rf_rules(input$rules.rf.n)
    }
  })
  
  # Upgrade code fields to default version
  deafult_codigo_rf <- function(){
    if(!is.null(datos.aprendizaje) & rf.args.default){
      mtry.value <- round(sqrt(ncol(datos.aprendizaje)))
      updateNumericInput(session,"mtry.rf",value = mtry.value)
      rf.args.default <<- FALSE
    }else{
      mtry.value <- input$mtry.rf
    }
    
    # Se acualiza el codigo del modelo
    codigo <- rf_model(variable.pred = variable.predecir,
                       ntree = input$ntree.rf,
                       mtry = mtry.value)
    
    updateAceEditor(session, "fieldCodeRf", value = codigo)
    cod.rf.modelo <<- codigo
    
    # Se genera el codigo de la prediccion
    codigo <- rf_prediction(variable.pred = variable.predecir)
    updateAceEditor(session, "fieldCodeRfPred", value = codigo)
    cod.rf.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models("prediccion.rf", translate("rfl"), variable.predecir)
    updateAceEditor(session, "fieldCodeRfDisp", value = codigo)
    
    # Cambia el codigo del grafico de rf
    updateAceEditor(session, "fieldCodeRfPlot", value = extract_code("importance_plot_rf"))
    
    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeRfIG", value = codigo)
    cod.rf.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_rf <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rf <<- NULL
        output$txtRf <- renderPrint(invisible(""))
      }, {
        prediccion.rf <<- NULL
        output$rfPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rf <<- rep(0, 10)
      })
    }
  }
  
  # Shows the chart of importance
  plotear_rf_imp <- function() {
    tryCatch({
      output$plot.rf <- renderPlot(isolate(importance_plot_rf(modelo.rf,translate("impVarA"),translate("impVarRSS"))))
    }, error = function(e) {
      output$plot.rf <- renderPlot(NULL)
    })
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rf <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.rf.disp <- renderPlot(exe(input$fieldCodeRfDisp))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rf(2)
      showNotification(paste0("Error (RF-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Show Rules
  show_rf_rules <- function(n){
    output$rulesRf <- renderPrint({
      tryCatch({
        updateAceEditor(session,"fieldCodeRfRules",paste0("printRandomForests(modelo.rf, ",n,", format='VB')"))
        printRandomForests(modelo.rf, n, format='VB')
      },error = function(e){
        stop(translate("NoDRule"))
      })
    })
  }
  
  # Execute model, prediction and indices
  rf_full <- function(){
    execute_rf()
    execute_rf_pred()
    execute_rf_ind()
  }
  
  # Generates the model
  execute_rf <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rf.modelo))
      output$txtRf <- renderPrint(print(modelo.rf))
      
      plotear_rf_imp()
      plot_disp_rf()
      show_rf_rules(input$rules.rf.n)
      #nombres.modelos <<- c(nombres.modelos, "modelo.rf")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rf(1)
      showNotification(paste0("Error (RF-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_rf_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rf.pred))
      
      output$rfPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.rf), server = FALSE)
      
      #nombres.modelos <<- c(nombres.modelos, "prediccion.rf")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rf(2)
      showNotification(paste0("Error (RF-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_rf_ind <- function() {
    if(exists("prediccion.rf") && !is.null(prediccion.rf)){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rf.ind))
        
        indices.rf <- general_indices(datos.prueba[,variable.predecir], prediccion.rf)
        
        df <- as.data.frame(indices.rf)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrf <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrf2 <- render_index_table(df2)
        
        #nombres.modelos <<- c(nombres.modelos, "indices.rf")
        updateData$IndicesM[["rfl"]] <<- indices.rf
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_rf(3)
        showNotification(paste0("Error (RF-03) : ",e), duration = 15, type = "error")
      })
    }
  }
}
    
## To be copied in the UI
# mod_random_forests_ui("random_forests_ui_1")
    
## To be copied in the server
# callModule(mod_random_forests_server, "random_forests_ui_1")
 
