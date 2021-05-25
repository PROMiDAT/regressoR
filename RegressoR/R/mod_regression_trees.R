#' regression_trees UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_regression_trees_ui <- function(id){
  
  ns <- NS(id)
  
  dt.options <- list(fluidRow(column(width = 9, h4(labelInput("opciones"))),
                              column(width = 2, br(),actionButton(ns("runDt"), label = labelInput("ejecutar"), icon = icon("play")))),
                     hr(),
                     fluidRow(column(numericInput(ns("minsplit.dt"), labelInput("minsplit"), 2, width = "100%",min = 1), width = 6),
                              column(numericInput(ns("maxdepth.dt"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1),width = 6)))
  
  dt.code <- list(h4(labelInput("codigo")), hr(),
                  conditionalPanel("input.BoxDt == 'tabDtModelo'",
                                   aceEditor(ns("fieldCodeDt"), mode = "r", theme = "monokai",
                                             value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtPlot'",
                                   aceEditor(ns("fieldCodeDtPlot"), mode = "r", theme = "monokai",
                                             value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtPred'",
                                   aceEditor(ns("fieldCodeDtPred"), mode = "r", theme = "monokai",
                                             value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtDisp'",
                                   aceEditor(ns("fieldCodeDtDisp"), mode = "r", theme = "monokai",
                                             value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtIndex'",
                                   aceEditor(ns("fieldCodeDtIG"), mode = "r", theme = "monokai",
                                             value = "", height = "22vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtReglas'",
                                   aceEditor(ns("fieldCodeDtRule"), mode = "r", theme = "monokai",
                                             value = "", height = "4vh", readOnly = F, autoComplete = "enabled"),ns = ns))
  
  tabs.dt <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                         tabs.content = list(dt.options, dt.code))
  
  generate.dt.panel <- tabPanel(title = labelInput("generatem"), value = "tabDtModelo",
                                verbatimTextOutput(ns("txtDt")))
  
  plot.dt <- tabPanel(title = labelInput("garbol"), value = "tabDtPlot",
                      plotOutput(ns('plot.dt'), height = "55vh"))
  
  prediction.dt.panel <- tabPanel(title = labelInput("predm"), value = "tabDtPred",
                                  DT::dataTableOutput(ns("dtPrediTable")))
  
  disp.dt.panel <- tabPanel(title = labelInput("dispersion"), value = "tabDtDisp",
                            plotOutput(ns('plot.dt.disp'), height = "55vh"))
  
  general.index.dt.panel <- tabPanel(title = labelInput("indices"),value = "tabDtIndex",
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfdt'))),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfdt2'))))
  
  rules.dt.panel <- tabPanel(title = labelInput("reglas"),value = "tabDtReglas",
                             verbatimTextOutput(ns("rulesDt")))
  
  page.dt <- tabItem(tabName = "dt",
                     tabBox(id = ns("BoxDt"), width = NULL, height ="80%",
                            generate.dt.panel,
                            plot.dt,
                            prediction.dt.panel,
                            disp.dt.panel,
                            general.index.dt.panel,
                            rules.dt.panel,
                            tabs.dt))
  
  tagList(
    page.dt
  )
}
    
#' regression_trees Server Function
#'
#' @noRd 
mod_regression_trees_server <- function(input, output, session,updateData, updatePlot){
  ns <- session$ns
  
  return.dt.default.values <- function(){
    updateNumericInput(session,inputId = "minsplit.dt", value = 2)
    updateNumericInput(session,inputId = "maxdepth.dt", value = 15)
    output$txtDt <- renderText(NULL)
    output$plot.dt <- renderPlot(NULL)
    output$dtPrediTable <- DT::renderDataTable(NULL)
    output$plot.dt.disp <- renderPlot(NULL)
    output$indexdfdt <- render_index_table(NULL)
    output$indexdfdt2 <- render_index_table(NULL)
    output$rulesDt <- renderText(NULL)
  }
  
  # change model codes
  observeEvent(updateData$datos.aprendizaje,{
    return.dt.default.values()
  })
  
  
  #  When the dt model is generated
  observeEvent(input$runDt, {
    if (validate_data()) { # Si se tiene los datos entonces :
      default_codigo_dt()
      dt_full()
    }
  })
  
  # When the user changes the parameters
  # observeEvent(c(input$minsplit.dt, input$maxdepth.dt), {
  #   if (validate_data(print = FALSE)){
  #     default_codigo_dt()
  #   }
  # })
  
  # Upgrade code fields to default version
  default_codigo_dt <- function() {
    
    # Se acualiza el codigo del modelo
    codigo <- dt_model(variable.pred =  variable.predecir,
                       minsplit = input$minsplit.dt,
                       maxdepth = input$maxdepth.dt)
    
    updateAceEditor(session, "fieldCodeDt", value = codigo)
    cod.dt.modelo <<- codigo
    
    # Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeDtPlot", value = dt_plot())
    
    # Se genera el codigo de la prediccion
    codigo <- dt_prediction()
    updateAceEditor(session, "fieldCodeDtPred", value = codigo)
    cod.dt.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models("prediccion.dt", translate("dtl"), variable.predecir)
    updateAceEditor(session, "fieldCodeDtDisp", value = codigo)
    
    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeDtIG", value = codigo)
    cod.dt.ind <<- codigo
  }
  
  # Shows the graph of the tree
  plot_tree <- function(){
    tryCatch({
      output$plot.dt <- renderPlot(isolate(exe(input$fieldCodeDtPlot)))
    },
    error = function(e){
      output$plot.dt <- renderPlot(NULL)
      #remove_report_elem("modelo.dt.graf")
    })
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_dt <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.dt.disp <- renderPlot(exe(input$fieldCodeDtDisp))
      #insert_report("disp.dt", "Dispersi\u00F3n del Modelo \u00C1rboles de Decisi\u00F3n", input$fieldCodeDtDisp)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_dt(2)
      showNotification(paste0("Error (DT-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Shows the rules of the tree
  show_dt_rules <- function(){
    output$rulesDt <- renderPrint(rattle::asRules(modelo.dt))
    updateAceEditor(session, "fieldCodeDtRule", paste0("asRules(modelo.dt)"))
    #insert_report("modelo.dt.rules", "Reglas del Modelo \u00C1rboles de Decisi\u00F3n", "rattle::asRules(modelo.dt)")
  }
  
  # Cleans the data according to the process where the error is generated
  clean_dt <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        modelo.dt <<- NULL
        output$txtDt <- renderPrint(invisible(""))
        output$plot.dt <- renderPlot(NULL)
        # remove_report_elem("modelo.dt")
        # remove_report_elem("modelo.dt.graf")
        # remove_report_elem("disp.dt")
      }, {
        prediccion.dt <<- NULL
        #remove_report_elem("pred.dt")
        output$dtPrediTable <- DT::renderDataTable(NULL)
      }, {
        indices.dt <<- rep(0, 10)
        #remove_report_elem("ind.dt")
      })
    }
  }
  
  # Execute model, prediction and indices
  dt_full <- function() {
    execute_dt()
    execute_dt_pred()
    execute_dt_ind()
  }
  
  # Generates the model
  execute_dt <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.dt.modelo))
      output$txtDt <- renderPrint(print(modelo.dt))
      #insert_report("modelo.dt", "Generaci\u00F3n del modelo \u00C1rboles de Decisi\u00F3n", cod.dt.modelo, "\nmodelo.dt")
      plot_tree()
      show_dt_rules()
      #nombres.modelos <<- c(nombres.modelos, "modelo.dt")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_dt(1)
      showNotification(paste0("Error (DT-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_dt_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.dt.pred))
      # Cambia la tabla con la prediccion de dt
      output$dtPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.dt),server = FALSE)
      
      # insert_report("pred.dt", "Predicci\u00F3n del Modelo \u00C1rboles de Decisi\u00F3n", 
      #               cod.dt.pred,"\nkt(head(tb_predic(real.val, prediccion.dt)$x$data[,-1]))",interpretation = FALSE)
      
      plot_disp_dt()
      #nombres.modelos <<- c(nombres.modelos, "prediccion.dt")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_dt(2)
      showNotification(paste0("Error (DT-02) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_dt_ind <- function() {
    if(exists("prediccion.dt") && !is.null(prediccion.dt)){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.dt.ind))
        
        indices.dt <- general_indices(datos.prueba[,variable.predecir], prediccion.dt)
        
        # insert_report("ind.dt", "\u00CDndices Generales del Modelo \u00C1rboles de Decisi\u00F3n",
        #               cod.dt.ind,"\nkt(general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.dt))\n",
        #               "indices.dt <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.dt)\n",
        #               "IndicesM[['dtl']] <- indices.dt")
        
        df <- as.data.frame(indices.dt)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfdt <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfdt2 <- render_index_table(df2)
        
        updateData$IndicesM[["dtl"]] <<- indices.dt
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_dt(3)
        showNotification(paste0("Error (DT-03) : ",e), duration = 15, type = "error")
      })
    }
  }
}
    
## To be copied in the UI
# mod_regression_trees_ui("regression_trees_ui_1")
    
## To be copied in the server
# callModule(mod_regression_trees_server, "regression_trees_ui_1")
 
