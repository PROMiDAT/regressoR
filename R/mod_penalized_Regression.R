#' penalized_Regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_penalized_Regression_ui <- function(id){
  ns <- NS(id)
  
  
  rlr.options <- list(options.run(ns("runRlr")), tags$hr(style = "margin-top: 0px;"),
                      fluidRow(column(selectInput(inputId = ns("alpha.rlr"), label = labelInput("selectAlg"), selected = 1,
                                                  choices = list("Ridge" = 0, "Lasso" = 1)),width = 6),
                               column(br(), switchInput(inputId = ns("switch.scale.rlr"), onStatus = "success", offStatus = "danger", value = T,
                                                        label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=6)),
                      fluidRow(column(id = ns("colManualLanda"),width = 5, numericInput(ns("landa"), labelInput("landa"),value = 2, "NULL", width = "100%")), br(),
                               column(width = 6, switchInput(inputId = ns("permitir.landa"), onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                             label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%"),
                                      style = "padding-top: 5px;")))
  
  
  rlr.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                          aceEditor(ns("fieldCodeRlr"), mode = "r", theme = "monokai",
                                    value = "", height = "12vh", readOnly = F, autoComplete = "enabled"))
  
  
  rlr.code  <- list(fluidRow(column(width = 9, h3(labelInput("codigo")))),
                    hr(style = "margin-top: 0px;"),
                    conditionalPanel("input.BoxRlr == 'tabRlrPosibLanda'",
                                     aceEditor(ns("fieldCodeRlrPosibLanda"), mode = "r", theme = "monokai",
                                               value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrCoeff_landa'",
                                     aceEditor(ns("fieldCodeRlrCoeff_landa"), mode = "r", theme = "monokai",
                                               value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrCoeff'",
                                     aceEditor(ns("fieldCodeRlrCoeff"), mode = "r", theme = "monokai",
                                               value = "", height = "12vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrPred'",
                                     aceEditor(ns("fieldCodeRlrPred"), mode = "r", theme = "monokai",
                                               value = "", height = "18vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrDisp'",
                                     aceEditor(ns("fieldCodeRlrDisp"), mode = "r", theme = "monokai",
                                               value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrIndex'",
                                     aceEditor(ns("fieldCodeRlrIG"), mode = "r", theme = "monokai",
                                               value = "", height = "22vh", readOnly = F, autoComplete = "enabled"),ns = ns))
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(80,70),
                          tabs.content = list(rlr.options,rlr.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                               tabs.content = list(rlr.code))
     
  
  generate.rlr.panel <- tabPanel(title = labelInput("generatem"),value = "tabRlrModelo",
                                 verbatimTextOutput(ns("txtRlr")))
  
  posib.landa.rlr.panel <- tabPanel(title = labelInput("posibLanda"),value = "tabRlrPosibLanda",
                                    echarts4rOutput(ns('plot.rlr.posiblanda'), height = "80vh"))
  
  coeff.rlr.panel <- tabPanel(title = labelInput("coeff"),value = "tabRlrCoeff",
                              verbatimTextOutput(ns("txtRlrCoeff")))
  
  landa.rlr.panel <- tabPanel(title = labelInput("gcoeff"),value = "tabRlrCoeff_landa",
                              echarts4rOutput(ns('plot.rlr.Coeff_landa'), height = "75vh"))
  
  prediccion.rlr.panel <- tabPanel(title = labelInput("predm"), value = "tabRlrPred",
                                   DT::dataTableOutput(ns("rlrPrediTable")))
  
  disp.rlr.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRlrDisp",
                             echarts4rOutput(ns('plot.rlr.disp'), height = "75vh"))
  
  rlr.general.index.panel <- tabPanel(title = labelInput("indices"), value = "tabRlrIndex",
                                      br(),
                                      fluidRow(tableOutput(ns('indexdfrlr'))),
                                      br(),
                                      fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                      br(),
                                      fluidRow(tableOutput(ns('indexdfrlr2'))))
  
  
  page.rlr <- tabItem(tabName = "rlr",
                      tabBox(id = ns("BoxRlr"), width = NULL, height ="80%",
                             generate.rlr.panel,
                             posib.landa.rlr.panel,
                             landa.rlr.panel,
                             coeff.rlr.panel,
                             prediccion.rlr.panel,
                             disp.rlr.panel,
                             rlr.general.index.panel,
                             conditionalPanel("input.BoxRlr == 'tabRlrModelo'",tabs.options.generate,ns = ns),
                             conditionalPanel("input.BoxRlr != 'tabRlrModelo'",tabs.options.Nogenerate,ns = ns)
                             ))
  
  tagList(
    page.rlr
  )
}
    
#' penalized_Regression Server Function
#'
#' @noRd 
mod_penalized_Regression_server <- function(input, output, session, updateData, updatePlot){
  ns <- session$ns
  
  return.rlr.default.values <- function(){
    updateSelectInput(session, "alpha.rlr",selected = 1)
    updateSwitchInput(session, "switch.scale.rlr", value = T)
    updateNumericInput(session,"landa",value = 2)
    updateSwitchInput(session, "permitir.landa", value = F)
    output$txtRlr <- renderText(NULL)
    output$plot.rlr.posiblanda <- renderPlot(NULL)
    output$txtRlrCoeff <- renderText(NULL)
    output$rlCoefTable <- DT::renderDataTable(NULL)
    output$plot.rlr.landa <- renderPlot(NULL)
    output$rlrPrediTable <- DT::renderDataTable(NULL)
    output$plot.rlr.disp <- renderEcharts4r(NULL)
    output$indexdfrlr <- render_index_table(NULL)
    output$indexdfrlr2 <- render_index_table(NULL)
  }
  
  observeEvent(updateData$idioma,{
    model.var = paste0("modelo.rlr.", rlr_type())
    if(exists(model.var)){
      execute_rlr_ind()
      plot_disp_rlr()
      plot_posib_landa_rlr()
      plot_coeff()
    }
  })
  

  observeEvent(updateData$datos.aprendizaje,{
    return.rlr.default.values()
  })
 
  # When the rlr model is generated
  observeEvent(input$runRlr, {
    if (validate_data()) { # If you have the data then :
      options_regressor(rlr.alpha = input$alpha.rlr)
      deafult_codigo_rlr()
      rlr_full()
    }
  })

  # When user press enable or disable the lambda
  observeEvent(input$permitir.landa, {
    if (input$permitir.landa) {
      shinyjs::enable("landa")
    } else {
      shinyjs::disable("landa")
    }
  })
  
  # Upgrade code fields to default version
  deafult_codigo_rlr <- function(){
    landa <- NULL
    
    if (input$permitir.landa) {
      if (!is.na(input$landa)) {
        landa <- input$landa
      }
    }
    
    # The model code is updated
    codigo <- rlr_model(variable.pred = variable.predecir,
                        model.var = paste0("modelo.rlr.", rlr_type()),
                        alpha = input$alpha.rlr,
                        standardize = input$switch.scale.rlr)
    
    updateAceEditor(session, "fieldCodeRlr", value = codigo)
    cod.rlr.modelo <<- codigo
    
    # The code that prints the coefficients is generated
    codigo <- coef_lambda(variable.pred = variable.predecir,
                          model.var = paste0("modelo.rlr.", rlr_type()),
                          lambda = landa)
    
    updateAceEditor(session, "fieldCodeRlrCoeff", value = codigo)
    
    
    # The prediction code is generated
    codigo <- rlr_prediction(variable.pred = variable.predecir,
                             model.var = paste0("modelo.rlr.", rlr_type()),
                             pred.var = paste0("prediccion.rlr.", rlr_type()),
                             lambda = landa)
    
    updateAceEditor(session, "fieldCodeRlrPred", value = codigo)
    cod.rlr.pred <<- codigo

    
    # The index code is generated
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeRlrIG", value = codigo)
    cod.rlr.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_rlr <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rlr <<- NULL
        output$txtRlr <- renderPrint(invisible(""))
      }, {
        prediccion.rlr <<- NULL
        output$rlrPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rlr <<- rep(0, 10)
      })
    }
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rlr <- function(){
    tryCatch({ # Se corren los codigo
      titulos <- c(
        tr("predvsreal", updateData$idioma),
        tr("realValue", updateData$idioma),
        tr("pred", updateData$idioma)
      )

      output$plot.rlr.disp <- renderEcharts4r(plot_real_prediction(datos.prueba[variable.predecir],
                                                                   exe(paste0("prediccion.rlr.",rlr_type()))
                                                                   ,translate("rlr"),titulos))
      
      codigo <- disp_models(paste0("prediccion.rlr.",rlr_type()), translate("rlr"), variable.predecir)
      updateAceEditor(session, "fieldCodeRlrDisp", value = codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Show the graph of the possible lambda
  plot_posib_landa_rlr <- function(){
    tryCatch({ # Se corren los codigo
      landa <- NULL
      
      if (input$permitir.landa) {
        if (!is.na(input$landa)) {
          landa <- input$landa
        }
      }
      
      titulos <- c(
        tr("MSE", updateData$idioma),
        tr("lowCurve", updateData$idioma),
        tr("uppCurve", updateData$idioma),
        tr("selected", updateData$idioma),
        tr("nonZeroCoeff", updateData$idioma)
      )
      output$plot.rlr.posiblanda <- renderEcharts4r(e_posib_lambda(exe("modelo.rlr.", rlr_type()), landa, titulos))
      
      # The code of the possible landa is generated
      param.lambda <- ifelse(is.null(landa),"",paste0(",",landa))
      codigo <- paste0("e_posib_lambda(modelo.rlr.",rlr_type(), param.lambda, ")")
      updateAceEditor(session, "fieldCodeRlrPosibLanda", value = codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Displays coefficients as text
  print_coeff <- function(){
    tryCatch({ # Se corren los codigo
      output$txtRlrCoeff <- renderPrint(print(isolate(exe(input$fieldCodeRlrCoeff))))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Show the graph of the coefficients
  plot_coeff <- function(){
    tryCatch({ # Se corren los codigo
      landa <- NULL
      
      if (input$permitir.landa) {
        if (!is.na(input$landa)) {
          landa <- input$landa
        }
      }
      
      titulos <- c(
        tr("coeff", updateData$idioma),
        tr("selected", updateData$idioma)
      )
      
      output$plot.rlr.Coeff_landa <- renderEcharts4r(e_coeff_landa(exe("modelo.rlr.",rlr_type()), landa, titulos))
      
      # The code of the coefficients is generated with the best lambda
      param.lambda <- ifelse(is.null(landa),"",paste0(",",landa))
      codigo <- paste0("e_coeff_landa(modelo.rlr.",rlr_type(), param.lambda, ")")
      updateAceEditor(session, "fieldCodeRlrCoeff_landa", value = codigo)
    },
    error = function(e){ # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  rlr_full <- function(){
    execute_rlr()
    execute_rlr_pred()
    execute_rlr_ind()
  }
  
  # Generates the model
  execute_rlr <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rlr.modelo))
      isolate(tipo <- rlr_type())
      output$txtRlr <- renderPrint(print(exe("modelo.rlr.",tipo)))
      
      plot_posib_landa_rlr()
      print_coeff()
      plot_coeff()
      
      #nombres.modelos <<- c(nombres.modelos, paste0("modelo.rlr.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(1)
      showNotification(paste0("Error (R/L-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_rlr_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rlr.pred))
      isolate(tipo <- rlr_type())
      output$rlrPrediTable <- DT::renderDataTable(tb_predic(real.val, exe("prediccion.rlr.",tipo)), server = FALSE)
      
      plot_disp_rlr()
      #nombres.modelos <<- c(nombres.modelos, "prediccion.rlr")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_rlr_ind <- function() {
    var.prediction.name <- paste0("prediccion.rlr.",rlr_type())
    if(exists(var.prediction.name)){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rlr.ind))
        
        indices.rlr <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.rlr.",rlr_type()))
        
        df <- as.data.frame(indices.rlr)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrlr <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrlr2 <- render_index_table(df2)
        
        updateData$IndicesM[[paste0("rlr-",rlr_type())]] <<-  indices.rlr
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_rlr(3)
        showNotification(paste0("Error (R/L-03) : ",e), duration = 15, type = "error")
      })
    }
  }
}
    
## To be copied in the UI
# mod_penalized_Regression_ui("penalized_Regression_ui_1")
    
## To be copied in the server
# callModule(mod_penalized_Regression_server, "penalized_Regression_ui_1")
 
