#' dimension_reduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dimension_reduction_ui <- function(id){
  
  rd.options  <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                               column(width = 2,br(),actionButton("runRd", label = labelInput("ejecutar"), icon = icon("play")))),
                      hr(),
                      fluidRow(column(selectInput(inputId = "modo.rd", label = labelInput("selectAlg"),selected = 0,
                                                  choices = list("ACP" = 0, "MCP" = 1)),width = 6),
                               column(br(), switchInput(inputId = "switch.scale.rd", onStatus = "success", offStatus = "danger", value = T,
                                                        label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=6)),
                      fluidRow(column(id = "colManualCom",width = 6, numericInput("ncomp.rd", labelInput("ncomp"),value = 2, min = 1, width = "100%")), br(),
                               column(width = 6, switchInput(inputId = "permitir.ncomp", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                             label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%"))))
  
  rd.code   <- list(fluidRow(column(width = 9,h4(labelInput("codigo")))),
                    hr(),
                    conditionalPanel("input.BoxRd == 'tabRdModelo'",
                                     aceEditor("fieldCodeRd", mode = "r", theme = "monokai",
                                               value = "", height = "8vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdRMSE'",
                                     aceEditor("fieldCodeRdRMSE", mode = "r", theme = "monokai",
                                               value = "", height = "14vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdPlotPred'",
                                     aceEditor("fieldCodeRdPlotPred", mode = "r", theme = "monokai",
                                               value = "", height = "14vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdPlotVarPred'",
                                     aceEditor("fieldCodeRdPlotVarPred", mode = "r", theme = "monokai",
                                               value = "", height = "14vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdPred'",
                                     aceEditor("fieldCodeRdPred", mode = "r", theme = "monokai",
                                               value = "", height = "10vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdDisp'",
                                     aceEditor("fieldCodeRdDisp", mode = "r", theme = "monokai",
                                               value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdIndex'",
                                     aceEditor("fieldCodeRdIG", mode = "r", theme = "monokai",
                                               value = "", height = "22vh", readOnly = F, autoComplete = "enabled")))
  
  tabs.rd  <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                          tabs.content = list(rd.options, rd.code))
  
  generate.rd.panel <- tabPanel(title = labelInput("generatem"),value = "tabRdModelo",
                                verbatimTextOutput("txtRd"))
  
  rmse.rd.panel <- tabPanel(title = labelInput("RMSE"),value = "tabRdRMSE",
                            plotOutput('plot.rd.rmse', height = "55vh"))
  
  plot.pred.rd.panel <- tabPanel(title = labelInput("RdPred"), value = "tabRdPlotPred",
                                 plotOutput('plot.rd.pred', height = "55vh"))
  
  panel.plot.var.pred.rd <- tabPanel(title = labelInput("RdVarPred"), value = "tabRdPlotVarPred",
                                     plotOutput('plot.rd.var.pred', height = "55vh"))
  
  prediction.rd.panel <- tabPanel(title = labelInput("predm"), value = "tabRdPred",
                                  DT::dataTableOutput("rdPrediTable"))
  
  disp.rd.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRdDisp",
                            plotOutput('plot.rd.disp', height = "55vh"))
  
  general.index.rd.panel <- tabPanel(title = labelInput("indices"), value = "tabRdIndex",
                                     br(),
                                     fluidRow(tableOutput('indexdfrd')),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(tableOutput('indexdfrd2')))
  
  page.rd <- tabItem(tabName = "rd",
                     tabBox(id = "BoxRd", width = NULL, height ="80%",
                            generate.rd.panel,
                            rmse.rd.panel,
                            plot.pred.rd.panel,
                            panel.plot.var.pred.rd,
                            prediction.rd.panel,
                            disp.rd.panel,
                            general.index.rd.panel,
                            tabs.rd))
  
  
  ns <- NS(id)
  tagList(
 
  )
}
    
#' dimension_reduction Server Function
#'
#' @noRd 
mod_dimension_reduction_server <- function(input, output, session){
  ns <- session$ns
  
  
  #  When the dt model is generated
  observeEvent(input$runRd, {
    if (validate_data()) { # Si se tiene los datos entonces :
      rd_full()
    }
  })
  
  # When the user changes the parameters
  observeEvent(c(input$modo.rd, input$switch.scale.rd, input$ncomp.rd, input$permitir.ncomp), {
    if (validate_data(print = FALSE)) {
      deafult_codigo_rd()
    }
  })
  
  # Habilitada o deshabilitada el número de componenetes 
  observeEvent(input$permitir.ncomp, {
    if (input$permitir.ncomp) {
      shinyjs::enable("ncomp.rd")
    } else {
      shinyjs::disable("ncomp.rd")
    }
  })
  
  # Upgrade code fields to default version
  deafult_codigo_rd <- function(){
    ncomp <- NULL
    if (input$permitir.ncomp) {
      if(!is.na(input$ncomp.rd) && input$ncomp.rd >= 0) {
        ncomp <- input$ncomp.rd
      }
    }
    
    options_regressor(rd.mode = input$modo.rd)
    
    # Se acualiza el codigo del modelo
    codigo <- rd_model(variable.pred = variable.predecir,
                       model.var = paste0("modelo.rd.",rd_type()),
                       n.comp = "n.comp.rd",
                       scale = input$switch.scale.rd)
    
    updateAceEditor(session, "fieldCodeRd", value = codigo)
    cod.rd.modelo <<- codigo
    
    # Se genera el codigo del plot de RMSE
    codigo <- extract_code("plot_RMSE")
    updateAceEditor(session, "fieldCodeRdRMSE", value = codigo)
    
    # Se genera el codigo del plot de predictoras
    codigo <- extract_code("plot_pred_rd")
    updateAceEditor(session, "fieldCodeRdPlotPred", value = codigo)
    
    # Se genera el codigo del plot de predictoras
    codigo <- extract_code("plot_var_pred_rd")
    updateAceEditor(session, "fieldCodeRdPlotVarPred", value = codigo)
    
    # Se genera el codigo de la prediccion
    codigo <- rd_prediction(model.var = paste0("modelo.rd.",rd_type()),
                            pred.var = paste0("prediccion.rd.",rd_type()),
                            n.comp  = "n.comp.rd",
                            ncomp = ncomp)
    updateAceEditor(session, "fieldCodeRdPred", value = codigo)
    cod.rd.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models(paste0("prediccion.rd.",rd_type()), translate("rd"), variable.predecir)
    updateAceEditor(session, "fieldCodeRdDisp", value = codigo)
    
    # Se genera el codigo de la indices 
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeRdIG", value = codigo)
    cod.rd.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_rd <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rd <<- NULL
        output$txtRd <- renderPrint(invisible(""))
        remove_report_elem(paste0("modelo.rd.",rd_type()))
        remove_report_elem(paste0("rmse.rd.",rd_type()))
        remove_report_elem(paste0("plot.pred.rd.",rd_type())) 
        remove_report_elem(paste0("plot.var.pred.rd.",rd_type()))
      }, {
        prediccion.rd <<- NULL
        remove_report_elem(paste0("pred.rd.",rd_type()))
        remove_report_elem(paste0("disp.rd.",rd_type())) 
        output$rdPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rd <<- rep(0, 10)
        remove_report_elem(paste0("ind.rd",rd_type()))
      })
    }
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rd <- function(){
    tryCatch({ # Se corren los codigo
      codigo <- input$fieldCodeRdDisp
      output$plot.rd.disp <- renderPlot(isolate(exe(codigo)))
      insert_report(paste0("disp.rd.",rd_type()),
                    paste0("Dispersi\u00F3n del Modelo Reducci\u00F3n de Dimensiones (",rd_type(),")"),
                    codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }
  
  plot_rmse_rd <- function(){
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rd_type())
      ncomp <- n.comp.rd
      if (input$permitir.ncomp) {
        if(!is.na(input$ncomp.rd) && input$ncomp.rd >= 0) {
          ncomp <- input$ncomp.rd
        }
      }
      output$plot.rd.rmse <- renderPlot(exe("plot_RMSE(modelo.rd.",tipo,",",ncomp,")"))
      insert_report(paste0("rmse.rd.",tipo),
                    "Error RMSE seg\u00fan N\u00famero de Componentes",
                    paste0("plot_RMSE(modelo.rd.",tipo,",",ncomp,")"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(1)
      showNotification(paste0("Error (RD-01) : ", e), duration = 15, type = "error")
    })
  }
  
  rd_plot_pred <- function(){
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rd_type())
      ncomp <- n.comp.rd
      if (input$permitir.ncomp) {
        if(!is.na(input$ncomp.rd) && input$ncomp.rd >= 0) {
          ncomp <- input$ncomp.rd
        }
      }
      output$plot.rd.pred <- renderPlot(exe("plot_pred_rd(modelo.rd.",tipo,",",ncomp,")"))
      insert_report(paste0("plot.pred.rd.",tipo), "Gr\u00e1fico de varianza explicada en los predictores",
                    paste0("plot_pred_rd(modelo.rd.",tipo,",",ncomp,")"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(1)
      showNotification(paste0("Error (RD-01) : ", e), duration = 15, type = "error")
    })
  }
  
  rd_plot_var_pred <- function(){
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rd_type())
      ncomp <- n.comp.rd
      if (input$permitir.ncomp) {
        if(!is.na(input$ncomp.rd) && input$ncomp.rd >= 0) {
          ncomp <- input$ncomp.rd
        }
      }
      output$plot.rd.var.pred <- renderPlot(exe("plot_var_pred_rd(modelo.rd.",tipo,",",ncomp,")"))
      insert_report(paste0("plot.var.pred.rd.",tipo), "Gr\u00e1fico de varianza explicada en la variable a predecir",
                    paste0("plot_var_pred_rd(modelo.rd.",tipo,",",ncomp,")"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(1)
      showNotification(paste0("Error (RD-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  rd_full <- function(){
    execute_rd()
    execute_rd_pred()
    execute_rd_ind()
  }
  
  # Generates the model
  execute_rd <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rd.modelo))
      isolate(tipo <- rd_type())
      output$txtRd <- renderPrint(print(exe("summary(modelo.rd.",tipo,")")))
      
      insert_report(paste0("modelo.rd.",tipo), paste0("Generaci\u00f3n del Modelo Reducci\u00f3n de Dimensiones(",tipo,")"),
                    cod.rd.modelo, "\nsummary(modelo.rd.",tipo,")")
      
      plot_rmse_rd()
      rd_plot_pred()
      rd_plot_var_pred()
      
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.rd.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(1)
      showNotification(paste0("Error (RD-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_rd_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rd_type())
      
      if (input$permitir.ncomp && input$ncomp.rd >= 0) {
        if (input$ncomp.rd > exe("modelo.rd.",tipo,"$ncomp")) {
          stop(tr("errRDnCom") , call. = FALSE)
        }
      }
      
      cod.rd.pred <- gsub("n.comp.rd", as.character(n.comp.rd), cod.rd.pred)
      updateAceEditor(session, "fieldCodeRdPred", value = cod.rd.pred)
      
      
      isolate(exe(cod.rd.pred))
      output$rdPrediTable <- DT::renderDataTable(tb_predic(real.val,exe("prediccion.rd.",tipo)), server = FALSE)
      
      insert_report(paste0("pred.rd.",tipo),
                    paste0("Predicci\u00f3n del Modelo Reducci\u00f3n de Dimensiones(",tipo,")"), 
                    cod.rd.pred, "\nkt(head(tb_predic(real.val, prediccion.rd.",tipo,")$x$data[,-1]))")
      
      plot_disp_rd()
      nombres.modelos <<- c(nombres.modelos, "prediccion.rd")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(2)
      showNotification(paste0("Error (RD-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_rd_ind <- function() {
    if(exists(paste0("prediccion.rd.",rd_type()))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rd.ind))
        indices.rd <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.rd.",rd_type()))
        
        insert_report(paste0("ind.rd.",rd_type()),"\u00cdndices Generales del Modelo Reducci\u00f3n de Dimensiones",
                      cod.rd.ind, 
                      "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rd.",rd_type(),"))\n",
                      "indices.rd <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rd.",rd_type(),")\n",
                      "IndicesM[['rd-",rd_type(),"']] <- indices.rd")
        
        df <- as.data.frame(indices.rd)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrd <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrd2 <- render_index_table(df2)
        
        IndicesM[[paste0("rd-",rd_type())]] <<- indices.rd
        update_comparative_selector()
      },
      error = function(e){ # Regresamos al estado inicial y mostramos un error
        clean_rd(3)
        showNotification(paste0("Error (RD-03) : ",e), duration = 15, type = "error")
      })
    }
  }
 
}
    
## To be copied in the UI
# mod_dimension_reduction_ui("dimension_reduction_ui_1")
    
## To be copied in the server
# callModule(mod_dimension_reduction_server, "dimension_reduction_ui_1")
 
