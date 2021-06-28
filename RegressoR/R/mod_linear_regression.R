#' linear_regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_linear_regression_ui <- function(id){
  ns <- NS(id)
  
  rl.code  <- list(
                   conditionalPanel("input.BoxRl == 'tabRlModelo'",
                                    options.run(ns("runRl")), tags$hr(style = "margin-top: 0px;"),
                                    aceEditor(ns("fieldCodeRl"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRl != 'tabRlModelo'", 
                                    h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),ns = ns),
                   conditionalPanel("input.BoxRl == 'tabRlCoef'",
                                    aceEditor(ns("fieldCodeRlCoef"), mode = "r", theme = "monokai",
                                              value = "", height = "18vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRl == 'tabRlPred'",
                                    aceEditor(ns("fieldCodeRlPred"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRl == 'tabRlDisp'",
                                    aceEditor(ns("fieldCodeRlDisp"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRl == 'tabRlIndex'",
                                    aceEditor(ns("fieldCodeRlIG"), mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled"),ns = ns))
  
  tabs.rl  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(95),
                          tabs.content = list(rl.code))
  
  generate.rl.panel <- tabPanel(title = labelInput("generatem"),value = "tabRlModelo",
                                verbatimTextOutput(ns("txtRl")))
  
  coefficients.rl.panel <- tabPanel(title = labelInput("coeff"), value = "tabRlCoef",
                                    DT::dataTableOutput(ns("rlCoefTable")))
  
  prediccion.rl.panel <- tabPanel(title = labelInput("predm"), value = "tabRlPred",
                                  DT::dataTableOutput(ns("rlPrediTable")))
  
  disp.rl.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRlDisp",
                            echarts4rOutput(ns('plot.rl.disp'), height = "75vh"))
  
  rl.general.index.panel <- tabPanel(title = labelInput("indices"), value = "tabRlIndex",
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfrl'))),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfrl2'))))
  
  
  page.rl <- tabItem(tabName = "rl",
                     tabBox(id = ns("BoxRl"), width = NULL, height ="80%",
                            generate.rl.panel,
                            coefficients.rl.panel,
                            prediccion.rl.panel,
                            disp.rl.panel,
                            rl.general.index.panel,
                            tabs.rl))
  
  tagList(
    page.rl
  )
}
    
#' linear_regression Server Function
#'
#' @noRd 
mod_linear_regression_server <- function(input, output, session, updateData, updatePlot){
  ns <- session$ns
  
  return.rl.default.values <- function(){
    output$txtRl <- renderText(NULL)
    output$rlCoefTable <- DT::renderDataTable(NULL)
    output$rlPrediTable <- DT::renderDataTable(NULL)
    output$plot.rl.disp <- renderPlot(NULL)
    output$indexdfrl <- render_index_table(NULL)
    output$indexdfrl2 <- render_index_table(NULL)
  }
  
  observeEvent(updateData$idioma,{
    execute_rl_ind()
    plot_disp_rl()
  })
  

  observeEvent(updateData$datos.aprendizaje,{
    #Change to default values
    return.rl.default.values()
  })
  
  # When the rl model is generated
  observeEvent(input$runRl, {
    if (validate_data()) { # Si se tiene los datos entonces :
      deafult_codigo_rl()
      rl_full()
    }
  })
  
  # Upgrade code fields to default version
  deafult_codigo_rl <- function(){
    # Se acualiza el codigo del modelo
    codigo <- rl_model(variable.pred = variable.predecir)
    
    updateAceEditor(session, "fieldCodeRl", value = codigo)
    cod.rl.modelo <<- codigo
    
    #Se genera el codigo de los coeficientes
    codigo <- rl_coeff()
    updateAceEditor(session, "fieldCodeRlCoef", value = codigo)
    
    
    # Se genera el codigo de la prediccion
    codigo <- rl_prediction()
    updateAceEditor(session, "fieldCodeRlPred", value = codigo)
    cod.rl.pred <<- codigo
    
    
    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeRlIG", value = codigo)
    cod.rl.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_rl <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rl <<- NULL
        output$txtRl <- renderPrint(invisible(""))
      }, {
        prediccion.rl <<- NULL
        output$rlPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rl <<- rep(0, 10)
      })
    }
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rl <- function(){
    tryCatch({ # Se corren los codigo
      titulos <- c(
        tr("predvsreal", updateData$idioma),
        tr("realValue", updateData$idioma),
        tr("pred", updateData$idioma)
      )

      output$plot.rl.disp <- renderEcharts4r(plot_real_prediction(datos.prueba[variable.predecir],
                                                                  prediccion.rl,translate("rll"),titulos))
      
      codigo <- disp_models("prediccion.rl", translate("rll"), variable.predecir)
      updateAceEditor(session, "fieldCodeRlDisp", value = codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(2)
      showNotification(paste0("Error (RL-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Displays model coefficients
  coefficients_rl <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(rl_coeff()))
      
      output$rlCoefTable <- render_table_data(df.rl[,c(1,4)], server = FALSE)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(1)
      showNotification(paste0("Error (RL-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  rl_full <- function(){
    execute_rl()
    execute_rl_pred()
    execute_rl_ind()
  }
  
  # Generates the model
  execute_rl <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rl.modelo))
      output$txtRl <- renderPrint(print(summary(modelo.rl)))
      
      coefficients_rl()
      
      #nombres.modelos <<- c(nombres.modelos, "modelo.rl")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(1)
      showNotification(paste0("Error (RL-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_rl_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rl.pred))
      
      output$rlPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.rl), server = FALSE)
      
      plot_disp_rl()
      
      #nombres.modelos <<- c(nombres.modelos, "prediccion.rl")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(2)
      showNotification(paste0("Error (RL-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_rl_ind <- function() {
    if(exists("prediccion.rl") && !is.null(prediccion.rl)){
      tryCatch({ # Se corren los codigo
        isolate(exe(input$fieldCodeRlCoef))
        isolate(exe(cod.rl.ind))
        
        indices.rl <- general_indices(datos.prueba[,variable.predecir], prediccion.rl)
        
        df <- cbind(as.data.frame(indices.rl), r2)
        df <- df[,c(1,2,3,5,4)]
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("R2"), translate("correlacion"))
        output$indexdfrl <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrl2 <- render_index_table(df2)
        
        #nombres.modelos <<- c(nombres.modelos, "indices.rl")
        updateData$IndicesM[["rll"]] <<- indices.rl
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_rl(3)
        showNotification(paste0("Error (RL-03) : ",e), duration = 15, type = "error")
      })
    }
  }
 
}
    
## To be copied in the UI
# mod_linear_regression_ui("linear_regression_ui_1")
    
## To be copied in the server
# callModule(mod_linear_regression_server, "linear_regression_ui_1")
 
