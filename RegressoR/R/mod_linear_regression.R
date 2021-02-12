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
    
#' linear_regression Server Function
#'
#' @noRd 
mod_linear_regression_server <- function(input, output, session){
  ns <- session$ns
  
  # When the rl model is generated
  observeEvent(input$runRl, {
    if (validate_data()) { # Si se tiene los datos entonces :
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
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models("prediccion.rl", translate("rll"), variable.predecir)
    updateAceEditor(session, "fieldCodeRlDisp", value = codigo)
    
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
        remove_report_elem("modelo.rl")
        remove_report_elem("disp.rl")
      }, {
        prediccion.rl <<- NULL
        remove_report_elem("pred.rl")
        output$rlPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rl <<- rep(0, 10)
        remove_report_elem("ind.rl")
      })
    }
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rl <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.rl.disp <- renderPlot(exe(input$fieldCodeRlDisp))
      insert_report("disp.rl", "Dispersi\u00F3n del Modelo Regresi\u00F3n Lineal", input$fieldCodeRlDisp)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(2)
      showNotification(paste0("Error (RL-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Displays model coefficients
  coefficients_rl <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(input$fieldCodeRlCoef))
      
      output$rlCoefTable <- regressoR::render_table_data(df.rl[,c(1,4)], server = FALSE)
      insert_report("coeff.rl", "Coeficientes del Modelo Regresi\u00F3n Lineal", input$fieldCodeRlCoef, "\ndf.rl")
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
      
      insert_report("modelo.rl","Generaci\u00F3n del Modelo Regresi\u00F3n Lineal", cod.rl.modelo,"\nsummary(modelo.rl)")
      coefficients_rl()
      
      nombres.modelos <<- c(nombres.modelos, "modelo.rl")
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
      
      insert_report("pred.rl", "Predicci\u00F3n del Modelo Regresi\u00F3n Lineal",cod.rl.pred,
                    "\nkt(head(tb_predic(real.val, prediccion.rl)$x$data[,-1]))", interpretation = FALSE)
      
      plot_disp_rl()
      
      nombres.modelos <<- c(nombres.modelos, "prediccion.rl")
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
        
        insert_report("ind.rl","\u00CDndices Generales del Modelo Regresi\u00F3n Lineal", cod.rl.ind,
                      "\nkt(general_indices(datos.prueba[,'", variable.predecir, "'], prediccion.rl))\n",
                      "indices.rl<- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rl)\n",
                      "IndicesM[['rll']] <- indices.rl")
        
        df <- cbind(as.data.frame(indices.rl), r2)
        df <- df[,c(1,2,3,5,4)]
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("R2"), translate("correlacion"))
        output$indexdfrl <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrl2 <- render_index_table(df2)
        
        nombres.modelos <<- c(nombres.modelos, "indices.rl")
        IndicesM[["rll"]] <<- indices.rl
        update_comparative_selector()
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
 
