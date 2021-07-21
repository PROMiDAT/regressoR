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
                      fluidRow(
                        column(selectInput(inputId = ns("alpha.rlr"), label = labelInput("selectAlg"), selected = 1,
                                           choices = list("Ridge" = 0, "Lasso" = 1)),width = 5),
                        column(width = 5,br(), radioSwitch(id = ns("switch.scale.rlr"), label = "escal", 
                                                 names = c("si", "no")), style = "margin-top: -20px;")),
                      fluidRow(column(id = ns("colManualLanda"),width = 5, 
                                      numericInput(ns("log_landa"), labelInput("log_landa"),value = 2, "NULL", width = "100%")),
                               br(),
                               column(width = 5, 
                                      radioSwitch(id = ns("permitir.landa"), label = "",
                                                  names = c("manual", "automatico"), val.def = FALSE), style = "margin-top: -15px;")))
  
  
  rlr.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                          codigo.monokai(ns("fieldCodeRlr"), height = "7vh"))
  
  
  rlr.code  <- list(fluidRow(column(width = 9, h3(labelInput("codigo")))),
                    hr(style = "margin-top: 0px;"),
                    conditionalPanel("input.BoxRlr == 'tabRlrPosibLanda'",
                                     aceEditor(ns("fieldCodeRlrPosibLanda"), mode = "r", theme = "monokai",
                                               value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrCoeff_landa'",
                                     codigo.monokai(ns("fieldCodeRlrCoeff_landa"), height = "7vh"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrCoeff'",
                                     codigo.monokai(ns("fieldCodeRlrCoeff"), height = "7vh"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrPred'",
                                     codigo.monokai(ns("fieldCodeRlrPred"), height = "7vh"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrDisp'",
                                     codigo.monokai(ns("fieldCodeRlrDisp"), height = "7vh"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrIndex'",
                                     codigo.monokai(ns("fieldCodeRlrIG"),height = "7vh"),ns = ns))
  
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
mod_penalized_Regression_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  
  nombreBase <- "modelo.rlr."
  nombreModelo <- "modelo.rlr."
  log.landa <- NULL
  coefficients <- NULL
  
  return.rlr.default.values <- function(){
    updateSelectInput(session, "alpha.rlr",selected = 1)
    updateSwitchInput(session, "switch.scale.rlr", value = T)
    updateNumericInput(session,"log_landa",value = 2)
    updateSwitchInput(session, "permitir.landa", value = F)
    # output$txtRlr <- renderText(NULL)
    # output$plot.rlr.posiblanda <- renderPlot(NULL)
    # output$txtRlrCoeff <- renderText(NULL)
    # output$rlCoefTable <- DT::renderDataTable(NULL)
    # output$plot.rlr.landa <- renderPlot(NULL)
    # output$rlrPrediTable <- DT::renderDataTable(NULL)
    # output$plot.rlr.disp <- renderEcharts4r(NULL)
    # output$indexdfrlr <- render_index_table(NULL)
    # output$indexdfrlr2 <- render_index_table(NULL)
    log.landa <<- NULL
    coefficients <<- NULL
  }
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.rlr.default.values()
  })
  
  # When the rlr model is generated
  observeEvent(input$runRlr, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # If you have the data then :
      rlr_full()
    }
  })
  
  # When user press enable or disable the lambda
  observeEvent(input$permitir.landa, {
    if (as.logical(input$permitir.landa)) {
      shinyjs::enable("log_landa")
    } else {
      shinyjs::disable("log_landa")
    }
  })
  
  # Execute model, prediction and indices
  rlr_full <- function(){
    tryCatch({
      
      isolate(datos.aprendizaje <- updateData$datos.aprendizaje)
      isolate(datos.prueba <- updateData$datos.prueba)
      isolate(variable.predecir <- updateData$variable.predecir)
      isolate(alpha <- as.numeric(input$alpha.rlr))
      isolate(standardize <- as.logical(input$switch.scale.rlr))

      nombreModelo <<- paste0(nombreBase, rlr_type(alpha))
      
      #Model generate
      modelo.rlr <- rlr_model(data = datos.aprendizaje, variable.pred = variable.predecir,
                              alpha = alpha, standardize = standardize)
      updateAceEditor(session, "fieldCodeRlr", value = codeRlr(variable.predecir,alpha,standardize))
      #Cambiamos la forma en que va aparecer el call
      modelo.rlr$call$standardize <- standardize
      modelo.rlr$call$alpha <- alpha
      
      if (isolate(as.logical(input$permitir.landa) && !is.na(input$log_landa))) {
        log.landa <<- isolate(input$log_landa)
      }
      else{log.landa <<- NULL}
      
      # Coefficients
      coefficients <<- coef_lambda(data = datos.aprendizaje, variable.pred = variable.predecir,
                                   model = modelo.rlr, log.lambda = log.landa)
      updateAceEditor(session, "fieldCodeRlrCoeff", value = codeRlrCoeff(variable.predecir,
                                                                         nombreModelo,log.landa))

      # Prediction
      prediccion.rlr <- rlr_prediction(datos.aprendizaje, datos.prueba, variable.predecir, 
                                       modelo.rlr, log.lambda = log.landa)
      updateAceEditor(session, "fieldCodeRlrPred", value = codeRlrPred(variable.predecir,
                                                                       nombreModelo,log.landa))
      
      
      #Indices
      indices.rlr <- general_indices(datos.prueba[,variable.predecir], prediccion.rlr)
      updateAceEditor(session, "fieldCodeRlrIG", value = codeRlrIG(variable.predecir))

      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$rlr[[nombreModelo]] <- list(modelo = modelo.rlr, prediccion = prediccion.rlr, indices = indices.rlr,
                                                  id = rlr_type(alpha), label = "rlr"))
    }, error = function(e){
      isolate(modelos$rlr[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (RLR-00) : ",e), duration = 10, type = "error")
    })
  }
  
  
  #Update model tab
  output$txtRlr <- renderPrint({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        modelos.rlr <- modelos$rlr[[nombreModelo]]$modelo
        print(modelos.rlr)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RLR-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  output$plot.rlr.posiblanda <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        titulos <- c(
          tr("MSE", updateData$idioma),
          tr("lowCurve", updateData$idioma),
          tr("uppCurve", updateData$idioma),
          tr("selected", updateData$idioma),
          tr("nonZeroCoeff", updateData$idioma)
        )
        
        param.lambda <- ifelse(is.null(log.landa),"",paste0(", log.lambda = ",log.landa))
        codigo <- paste0("e_posib_lambda(", nombreModelo, param.lambda, ")")
        updateAceEditor(session, "fieldCodeRlrPosibLanda", value = codigo)
        
        e_posib_lambda(modelos$rlr[[nombreModelo]]$modelo, log.landa, titulos)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RLR-02) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  
  output$plot.rlr.Coeff_landa <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        param.lambda <- ifelse(is.null(log.landa),"",paste0(", log.lambda = ",log.landa))
        codigo <- paste0("e_coeff_landa(", nombreModelo, param.lambda, ")")
        updateAceEditor(session, "fieldCodeRlrCoeff_landa", value = codigo)
        
        titulos <- c(
          tr("coeff", updateData$idioma),
          tr("selected", updateData$idioma)
        )
        
        e_coeff_landa(modelos$rlr[[nombreModelo]]$modelo, log.landa, titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RLR-03) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update coefficients tab
  output$txtRlrCoeff <- renderPrint({
    tryCatch({
      ifelse(!is.null(modelos$rlr[[nombreModelo]]), print(coefficients), NULL)
    }, error = function(e){
      showNotification(paste0("Error (RLR-04) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Update prediction tab
  output$rlrPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        prediccion.rlr <- modelos$rlr[[nombreModelo]]$prediccion
        isolate(datos.prueba <- updateData$datos.prueba)
        isolate(real.val <- datos.prueba[updateData$variable.predecir])
        tb_predic(real.val, prediccion.rlr, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RLR-05) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update dispersion tab
  output$plot.rlr.disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        prediccion.rlr <- modelos$rlr[[nombreModelo]]$prediccion
        isolate(datos.prueba <- updateData$datos.prueba)
        isolate(variable.predecir <- updateData$variable.predecir)
        
        codigo <- disp_models(nombreModelo, tr("rlr", updateData$idioma), variable.predecir)
        updateAceEditor(session, "fieldCodeRlrDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", updateData$idioma),
          tr("realValue", updateData$idioma),
          tr("pred", updateData$idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.rlr,tr("rlr", updateData$idioma),titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RLR-06) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfrlr <- renderTable({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        idioma <- updateData$idioma
        indices.rlr <- modelos$rlr[[nombreModelo]]$indices
        df <- as.data.frame(indices.rlr)
        colnames(df) <- c(tr("RMSE", idioma), tr("MAE", idioma), tr("ER", idioma), tr("correlacion", idioma))
        df
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RL-07) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',  digits = 5,align = 'c')
  
  
  
  output$indexdfrlr2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        idioma <- updateData$idioma
        isolate(datos.prueba <- updateData$datos.prueba)
        isolate(variable.predecir <- updateData$variable.predecir)
        df2 <- as.data.frame(summary_indices(datos.prueba[,variable.predecir]))
        colnames(df2) <- c(tr("minimo",idioma),tr("q1",idioma),
                           tr("q3",idioma),tr("maximo",idioma))
        df2
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (RL-08) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',  digits = 5,align = 'c')
}

## To be copied in the UI
# mod_penalized_Regression_ui("penalized_Regression_ui_1")

## To be copied in the server
# callModule(mod_penalized_Regression_server, "penalized_Regression_ui_1")