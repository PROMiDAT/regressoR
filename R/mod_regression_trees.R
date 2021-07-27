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
  
  dt.options <- list(options.run(ns("runDt")), tags$hr(style = "margin-top: 0px;"),
                     fluidRow(column(numericInput(ns("minsplit.dt"), labelInput("minsplit"), 2, width = "100%",min = 1), width = 6),
                              column(numericInput(ns("maxdepth.dt"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1),width = 6)))
  
  dt.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                         codigo.monokai(ns("fieldCodeDt"), height = "7vh"))
  
  dt.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                  conditionalPanel("input.BoxDt == 'tabDtPlot'",
                                   codigo.monokai(ns("fieldCodeDtPlot"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtPred'",
                                   codigo.monokai(ns("fieldCodeDtPred"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtDisp'",
                                   codigo.monokai(ns("fieldCodeDtDisp"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtIndex'",
                                   codigo.monokai(ns("fieldCodeDtIG"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtReglas'",
                                   codigo.monokai(ns("fieldCodeDtRule"), height = "7vh"),ns = ns))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(70,70),
                                       tabs.content = list(dt.options,dt.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(dt.code))
  
  generate.dt.panel <- tabPanel(title = labelInput("generatem"), value = "tabDtModelo",
                                verbatimTextOutput(ns("txtDt")))
  
  plot.dt <- tabPanel(title = labelInput("garbol"), value = "tabDtPlot",
                      plotOutput(ns('plot.dt'), height = "75vh"))
  
  prediction.dt.panel <- tabPanel(title = labelInput("predm"), value = "tabDtPred",
                                  DT::dataTableOutput(ns("dtPrediTable")))
  
  disp.dt.panel <- tabPanel(title = labelInput("dispersion"), value = "tabDtDisp",
                            echarts4rOutput(ns('plot.dt.disp'), height = "75vh"))
  
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
                            conditionalPanel("input.BoxDt == 'tabDtModelo'",tabs.options.generate,ns = ns),
                            conditionalPanel("input.BoxDt != 'tabDtModelo'",tabs.options.Nogenerate,ns = ns)))
  
  tagList(
    page.dt
  )
}

#' regression_trees Server Function
#'
#' @noRd 
mod_regression_trees_server <- function(input, output, session,updateData, modelos){
  ns <- session$ns
  
  nombreModelo <- "modelo.dt"
  
  return.dt.default.values <- function(){
    updateNumericInput(session,inputId = "minsplit.dt", value = 2)
    updateNumericInput(session,inputId = "maxdepth.dt", value = 15)
    # output$txtDt <- renderText(NULL)
    # output$plot.dt <- renderPlot(NULL)
    # output$dtPrediTable <- DT::renderDataTable(NULL)
    # output$plot.dt.disp <- renderEcharts4r(NULL)
    # output$indexdfdt <- render_index_table(NULL)
    # output$indexdfdt2 <- render_index_table(NULL)
    # output$rulesDt <- renderText(NULL)
  }
  
  observeEvent(updateData$datos.aprendizaje,{
    return.dt.default.values()
  })
  
  
  #  When the dt model is generated
  observeEvent(input$runDt, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # Si se tiene los datos entonces :
      dt_full()
    }
  })
  
  # Execute model, prediction and indices
  dt_full <- function() {
    tryCatch({
      isolate(datos.aprendizaje <- updateData$datos.aprendizaje)
      isolate(datos.prueba <- updateData$datos.prueba)
      isolate(variable.predecir <- updateData$variable.predecir)
      
      ms <- isolate(input$minsplit.dt)
      md <- isolate(input$maxdepth.dt)
      minsplit <- ifelse(!is.numeric(ms), 20, ms)
      maxdepth <- ifelse(!is.numeric(md), 15, md)
      
      # Model Generate
      modelo.dt <- dt_model(datos.aprendizaje, variable.predecir,
                            minsplit = minsplit,
                            maxdepth = maxdepth)
      updateAceEditor(session, "fieldCodeDt", value = codeDt(variable.predecir,minsplit,maxdepth))
      
      #Prediccion
      prediccion.dt <- dt_prediction(modelo.dt,datos.prueba)
      updateAceEditor(session, "fieldCodeDtPred", value = codeDtPred(nombreModelo))
      
      #Indices
      indices.dt <- general_indices(datos.prueba[,variable.predecir], prediccion.dt)
      updateAceEditor(session, "fieldCodeDtIG", value = codeDtIG(variable.predecir))
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$dt[[nombreModelo]] <- list(modelo = modelo.dt, prediccion = prediccion.dt, indices = indices.dt, 
                                                 id = NULL))
      
    }, error = function(e){
      isolate(modelos$dt[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (DT-00) : ",e), duration = 10, type = "error")
    })
  }
  
  
  #Update model tab
  output$txtDt <- renderPrint({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        modelo.dt <- modelos$dt[[nombreModelo]]$modelo
        print(modelo.dt)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (DT-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Shows the graph of the tree
  output$plot.dt <- renderPlot({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        updateAceEditor(session, "fieldCodeDtPlot", value = codeDtPlot(nombreModelo))
        modelo.dt <- modelos$dt[[nombreModelo]]$modelo
        dt_plot(modelo.dt)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (DT-02) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Update prediction tab
  output$dtPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        prediccion.dt <- modelos$dt[[nombreModelo]]$prediccion
        isolate(datos.prueba <- updateData$datos.prueba)
        isolate(real.val <- datos.prueba[updateData$variable.predecir])
        tb_predic(real.val, prediccion.dt, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (DT-03) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)

  
  # Update dispersion tab
  output$plot.dt.disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        prediccion.dt <- modelos$dt[[nombreModelo]]$prediccion
        isolate(datos.prueba <- updateData$datos.prueba)
        isolate(variable.predecir <- updateData$variable.predecir)
        
        codigo <- disp_models("prediccion.dt", tr("dt",updateData$idioma), variable.predecir)
        updateAceEditor(session, "fieldCodeDtDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", updateData$idioma),
          tr("realValue", updateData$idioma),
          tr("pred", updateData$idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.dt,tr("dt", updateData$idioma),titulos)
      }
      else{NULL}
    },
    error = function(e) {
      showNotification(paste0("Error (DT-04) : ", e), duration = 15, type = "error")
      NULL
    })
  })
  
  
  # Update Rules tab
  output$rulesDt <- renderPrint({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        updateAceEditor(session, "fieldCodeDtRule", codeDtRule(nombreModelo))
        modelo.dt <- modelos$dt[[nombreModelo]]$modelo
        rattle::asRules(modelo.dt)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (DT-05) : ", e), duration = 15, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfdt <- renderTable({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        idioma <- updateData$idioma
        indices.dt <- modelos$dt[[nombreModelo]]$indices
        df <- as.data.frame(indices.dt)
        colnames(df) <- c(tr("RMSE", idioma), tr("MAE", idioma), tr("ER", idioma), tr("correlacion", idioma))
        df
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (DT-06) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',  digits = 5,align = 'c')
  
  
  
  output$indexdfdt2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
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
      showNotification(paste0("Error (DT-07) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',  digits = 5,align = 'c')
  
}

## To be copied in the UI
# mod_regression_trees_ui("regression_trees_ui_1")

## To be copied in the server
# callModule(mod_regression_trees_server, "regression_trees_ui_1")

