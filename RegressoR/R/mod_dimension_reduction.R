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
  
  ns <- NS(id)
  
  rd.options  <- list(options.run(ns("runRd")), tags$hr(style = "margin-top: 0px;"),
                      fluidRow(column(selectInput(inputId = ns("modo.rd"), label = labelInput("selectAlg"),selected = 0,
                                                  choices = list("ACP" = 0, "MCP" = 1)),width = 6),
                               column(br(), switchInput(inputId = ns("switch.scale.rd"), onStatus = "success", offStatus = "danger", value = T,
                                                        label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=6)),
                      fluidRow(column(id = ns("colManualCom"),width = 6, numericInput(ns("ncomp.rd"), labelInput("ncomp"),value = 2, min = 1, width = "100%")), br(),
                               column(width = 6, switchInput(inputId = ns("permitir.ncomp"), onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                             label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%"))))
  
  
  rd.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                         aceEditor(ns("fieldCodeRd"), mode = "r", theme = "monokai",
                                   value = "", height = "8vh", readOnly = F, autoComplete = "enabled"))
  
  
  rd.code   <- list(fluidRow(column(width = 9,h3(labelInput("codigo")))),
                    hr(style = "margin-top: 0px;"),
                    conditionalPanel("input.BoxRd == 'tabRdRMSE'",
                                     aceEditor(ns("fieldCodeRdRMSE"), mode = "r", theme = "monokai",
                                               value = "", height = "22vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdPlotPred'",
                                     aceEditor(ns("fieldCodeRdPlotPred"), mode = "r", theme = "monokai",
                                               value = "", height = "22vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdPlotVarPred'",
                                     aceEditor(ns("fieldCodeRdPlotVarPred"), mode = "r", theme = "monokai",
                                               value = "", height = "22vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdPred'",
                                     aceEditor(ns("fieldCodeRdPred"), mode = "r", theme = "monokai",
                                               value = "", height = "10vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdDisp'",
                                     aceEditor(ns("fieldCodeRdDisp"), mode = "r", theme = "monokai",
                                               value = "", height = "3vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdIndex'",
                                     aceEditor(ns("fieldCodeRdIG"), mode = "r", theme = "monokai",
                                               value = "", height = "22vh", readOnly = F, autoComplete = "enabled"), ns = ns))
  

  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(80,95),
                                       tabs.content = list(rd.options,rd.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(95),
                                         tabs.content = list(rd.code))
  
  
  generate.rd.panel <- tabPanel(title = labelInput("generatem"),value = "tabRdModelo",
                                verbatimTextOutput(ns("txtRd")))
  
  rmse.rd.panel <- tabPanel(title = labelInput("RMSE"),value = "tabRdRMSE",
                            plotOutput(ns('plot.rd.rmse'), height = "55vh"))
  
  plot.pred.rd.panel <- tabPanel(title = labelInput("RdPred"), value = "tabRdPlotPred",
                                 plotOutput(ns('plot.rd.pred'), height = "55vh"))
  
  panel.plot.var.pred.rd <- tabPanel(title = labelInput("RdVarPred"), value = "tabRdPlotVarPred",
                                     plotOutput(ns('plot.rd.var.pred'), height = "55vh"))
  
  prediction.rd.panel <- tabPanel(title = labelInput("predm"), value = "tabRdPred",
                                  DT::dataTableOutput(ns("rdPrediTable")))
  
  disp.rd.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRdDisp",
                            plotOutput(ns('plot.rd.disp'), height = "55vh"))
  
  general.index.rd.panel <- tabPanel(title = labelInput("indices"), value = "tabRdIndex",
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfrd'))),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfrd2'))))
  
  page.rd <- tabItem(tabName = "rd",
                     tabBox(id = ns("BoxRd"), width = NULL, height ="80%",
                            generate.rd.panel,
                            rmse.rd.panel,
                            plot.pred.rd.panel,
                            panel.plot.var.pred.rd,
                            prediction.rd.panel,
                            disp.rd.panel,
                            general.index.rd.panel,
                            conditionalPanel("input.BoxRd == 'tabRdModelo'",tabs.options.generate,ns = ns),
                            conditionalPanel("input.BoxRd != 'tabRdModelo'",tabs.options.Nogenerate,ns = ns)))
  
  
  tagList(
    page.rd
  )
}
    
#' dimension_reduction Server Function
#'
#' @noRd 
mod_dimension_reduction_server <- function(input, output, session,updateData, updatePlot){
  ns <- session$ns
  
  return.rd.default.values <- function(){
    updateSelectInput(session,"modo.rd",selected = 0)
    updateSwitchInput(session,"switch.scale.rd", value = T)
    updateNumericInput(session,"ncomp.rd", value = 2)
    updateSwitchInput(session,"permitir.ncomp", value = F)
    
    output$txtRd <- renderText(NULL)
    output$plot.rd.rmse <- renderPlot(NULL)
    output$plot.rd.pred <- renderPlot(NULL)
    output$plot.rd.var.pred <- renderPlot(NULL)
    output$rdPrediTable <- DT::renderDataTable(NULL)
    output$plot.rd.disp <- renderPlot(NULL)
    output$indexdfrd <- render_index_table(NULL)
    output$indexdfrd2 <- render_index_table(NULL)
  }
  
  observeEvent(updateData$idioma,{
    execute_rd_ind()
  })
  
  observeEvent(updateData$datos.aprendizaje,{
    return.rd.default.values()
  })
  
  #  When the dt model is generated
  observeEvent(input$runRd, {
    if (validate_data()) { # Si se tiene los datos entonces :
      deafult_codigo_rd()
      rd_full()
    }
  })
  
  # When the user changes the parameters
  # observeEvent(c(input$modo.rd, input$switch.scale.rd, input$ncomp.rd, input$permitir.ncomp), {
  #   if (validate_data(print = FALSE)) {
  #     deafult_codigo_rd()
  #   }
  # })
  
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
      }, {
        prediccion.rd <<- NULL
        output$rdPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rd <<- rep(0, 10)
      })
    }
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rd <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.rd.disp <- renderPlot(isolate(exe(input$fieldCodeRdDisp)))
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
      
      plot_rmse_rd()
      rd_plot_pred()
      rd_plot_var_pred()
      
      #nombres.modelos <<- c(nombres.modelos, paste0("modelo.rd.",tipo))
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
      
      plot_disp_rd()
      #nombres.modelos <<- c(nombres.modelos, "prediccion.rd")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(2)
      showNotification(paste0("Error (RD-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_rd_ind <- function() {
    var.prediction.name <- paste0("prediccion.rd.",rd_type())
    if(exists(var.prediction.name)){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rd.ind))
        indices.rd <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.rd.",rd_type()))
        
        df <- as.data.frame(indices.rd)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrd <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrd2 <- render_index_table(df2)
        
        updateData$IndicesM[[paste0("rd-",rd_type())]] <<- indices.rd
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
 
