#' neural_networks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_neural_networks_ui <- function(id){
  
  ns <- NS(id)
  
  nn.options <- list(options.run(ns("runNn")), tags$hr(style = "margin-top: 0px;"),
                     fluidRow(column(numericInput(ns("threshold.nn"),labelInput("threshold"),
                                                  min = 0, step = 0.01, value = 0.05), width = 6),
                              column(numericInput(ns("stepmax.nn"),labelInput("stepmax"),
                                                  min = 100, step = 100, value = 5000), width = 6)),
                     fluidRow(column(sliderInput(inputId = ns("cant.capas.nn"), min = 1, max = 10,
                                                 label = labelInput("selectCapas"), value = 2), width = 12)),
                     fluidRow(lapply(1:10, function(i) tags$span(numericInput(paste0(ns("nn.cap."),i), NULL,
                                                                              min = 1, step = 1, value = 2),
                                                                 class = "mini-numeric-select"))))
  
  nn.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                         aceEditor(ns("fieldCodeNn"), mode = "r", theme = "monokai", value = "", height = "22vh", readOnly = F))
  
  
  nn.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                  conditionalPanel("input.BoxNn == 'tabNnPlot'",
                                   aceEditor(ns("fieldCodeNnPlot"), mode = "r", theme = "monokai", value = "", height = "9vh", readOnly = F),ns = ns),
                  conditionalPanel("input.BoxNn == 'tabNnPred'",
                                   aceEditor(ns("fieldCodeNnPred"), mode = "r", theme = "monokai",
                                             value = "", height = "10vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxNn == 'tabNnDisp'",
                                   aceEditor(ns("fieldCodeNnDisp"), mode = "r", theme = "monokai",
                                             value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                  conditionalPanel("input.BoxNn == 'tabNnIndex'",
                                   aceEditor(ns("fieldCodeNnIG"), mode = "r", theme = "monokai",
                                             value = "", height = "22vh", readOnly = F, autoComplete = "enabled"),ns = ns))
  
  tabs.nn <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(75,100), heights = c(95, 95),
                         tabs.content = list(nn.options, nn.code))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(80,95),
                                       tabs.content = list(nn.options,nn.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(95),
                                         tabs.content = list(nn.code))
  
  
  plot.nn <- tabPanel(title = labelInput("redPlot"), value = "tabNnPlot",
                      plotOutput(ns('plot.nn'), height = "55vh"))
  
  generate.nn.panel <- tabPanel(title = labelInput("generatem"), value = "tabNnModelo",
                                verbatimTextOutput(ns("txtnn")))
  
  prediction.nn.panel <- tabPanel(title = labelInput("predm"), value = "tabNnPred",
                                  DT::dataTableOutput(ns("nnPrediTable")))
  
  disp.nn.panel <- tabPanel(title = labelInput("dispersion"), value = "tabNnDisp",
                            echarts4rOutput(ns('plot.nn.disp'), height = "75vh"))
  
  general.index.nn.panel <- tabPanel(title = labelInput("indices"), value = "tabNnIndex",
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfnn'))),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfnn2'))))
  
  page.nn  <- tabItem(tabName = "nn",
                      tabBox(id = ns("BoxNn"), width = NULL, height ="80%",
                             generate.nn.panel,
                             plot.nn,
                             prediction.nn.panel,
                             disp.nn.panel,
                             general.index.nn.panel,
                             conditionalPanel("input.BoxNn == 'tabNnModelo'",tabs.options.generate,ns = ns),
                             conditionalPanel("input.BoxNn != 'tabNnModelo'",tabs.options.Nogenerate,ns = ns)))
  
  
  tagList(
    page.nn
  )
}
    
#' neural_networks Server Function
#'
#' @noRd 
mod_neural_networks_server <- function(input, output, session,updateData, updatePlot){
  ns <- session$ns
  
  observeEvent(updateData$datos.aprendizaje, {
    #Change to default values
    return.nn.default.values()
  })
  
  
  # When user change the layer selector
  observeEvent(input$cant.capas.nn, {
    updateLayers()
  })
  
  return.nn.default.values <- function(){
    updateSliderInput(session, "cant.capas.nn", value = 2)
    updateNumericInput(session, "threshold.nn", value = 0.05)
    updateNumericInput(session, "stepmax.nn", value = 5000)
    updateLayers()
    output$plot.nn <- renderPlot(NULL)
    output$nnPrediTable <- DT::renderDataTable(NULL)
    output$plot.nn.disp <- renderEcharts4r(NULL)
    output$indexdfnn <- render_index_table(NULL)
    output$indexdfnn2 <- render_index_table(NULL)
  }
  
  
  observeEvent(updateData$idioma,{
    model.var = "modelo.nn"
    if(exists(model.var)){
      execute_nn_ind()
      plot_disp_nn()
    }
  })
  
  
  updateLayers <- function(){
    if(!is.null(datos.aprendizaje) && !is.null(input$cant.capas.nn)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn) {
          #No se usa ns() x el parámetro asis de show y hide.
          shinyjs::show(paste0("nn.cap.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.", i))
        }
      }
    }
  }
  
  # When the nn model is generated
  observeEvent(input$runNn, {
    if (validate_data()) { # Si se tiene los datos entonces :
      default_codigo_nn()
      nn_full()
    }
  })
  
  
  # Upgrade code fields to default version
  default_codigo_nn <- function(){
    #Se acualiza el codigo del modelo
    
    codigo <- nn_model(data = "datos.aprendizaje",
                       variable.pred = variable.predecir,
                       model.var = "modelo.nn",
                       mean.var = "mean.nn",
                       sd.var = "sd.nn",
                       threshold = input$threshold.nn,
                       stepmax = input$stepmax.nn,
                       cant.hidden = input$cant.capas.nn,
                       input$nn.cap.1,input$nn.cap.2,
                       input$nn.cap.3,input$nn.cap.4,
                       input$nn.cap.5,input$nn.cap.6,
                       input$nn.cap.7,input$nn.cap.8,
                       input$nn.cap.9,input$nn.cap.10)
    
    updateAceEditor(session, "fieldCodeNn", value = codigo)
    cod.nn.modelo <<- codigo
    
    #Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeNnPlot", value = nn_plot())
    
    #Se genera el codigo de la prediccion
    codigo <- nn_prediction(variable.pred = variable.predecir)
    updateAceEditor(session, "fieldCodeNnPred", value = codigo)
    cod.nn.pred <<- codigo
    
    #Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeNnIG", value = codigo)
    cod.nn.ind <<- codigo
  }
  
  # Shows the graph of the network
  plot_net <- function(){
    tryCatch({
      capas <- c(input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,
                 input$nn.cap.5,input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,input$nn.cap.9,input$nn.cap.10)
      capas <- capas[1:input$cant.capas.nn]
      if(input$cant.capas.nn * sum(capas) <= 1000 & ncol(modelo.nn$covariate) <= 25){
        codigo = nn_plot()
        output$plot.nn <- renderPlot(isolate(exe(codigo)))
      }else{
        showNotification(translate("bigPlot"), duration = 10, type = "message")
      }
    },
    error = function(e){
      output$plot.nn <- renderPlot(NULL)
    })
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_nn <- function(){
    tryCatch({ # Se corren los codigo
      titulos <- c(
        tr("predvsreal", updateData$idioma),
        tr("realValue", updateData$idioma),
        tr("pred", updateData$idioma)
      )
      
      output$plot.nn.disp <- renderEcharts4r(plot_real_prediction(datos.prueba[variable.predecir],
                                                                  prediccion.nn,translate("nn"),titulos))
      
      codigo <- disp_models("prediccion.nn", translate("nn"), variable.predecir)
      updateAceEditor(session, "fieldCodeNnDisp", value = codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_nn(2)
      showNotification(paste0("Error (NN-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Cleans the data according to the process where the error is generated
  clean_nn <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        exe("modelo.nn <- NULL")
        output$txtnn <- renderPrint(invisible(""))
        output$plot.nn <- renderPlot(NULL)
      }, {
        exe("prediccion.nn <- NULL")
        output$nnPrediTable <- DT::renderDataTable(NULL)
      },{
        exe("indices.nn <- NULL")
      })
    }
  }
  
  # Execute model, prediction and indices
  nn_full <- function() {
    execute_nn()
    if(NN_EXECUTION){
      execute_nn_pred()
      execute_nn_ind()
    }
  }
  
  # Generates the model
  execute_nn <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.nn.modelo))
      output$txtnn <- renderPrint(print(modelo.nn))
      plot_net()
      #nombres.modelos <<- c(nombres.modelos,"modelo.nn")
      NN_EXECUTION <<- TRUE
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_nn(1)
      showNotification(paste0("Error (NN-01) : ",e), duration = 15, type = "error")
    },
    warning = function(w){
      clean_nn(1)
      NN_EXECUTION <<- FALSE
      showNotification(paste0(translate("nnWar")," (NN-01) : ",w), duration = 20, type = "warning")
    },
    #Por si no converge
    finally = {
      output$plot.nn.disp <- renderEcharts4r(NULL)
      output$indexdfnn <- render_index_table(NULL)
      output$indexdfnn2 <- render_index_table(NULL)
    })
  }
  
  # Generate the prediction
  execute_nn_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.nn.pred))
      
      # Cambia la tabla con la prediccion de nn
      output$nnPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.nn),server = FALSE)
      
      plot_disp_nn()
      #nombres.modelos <<- c(nombres.modelos,"prediccion.nn")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comparativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_nn(2)
      showNotification(paste0("Error (NN-02) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_nn_ind <- function() {
    if(exists("prediccion.nn") && !is.null(prediccion.nn)){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.nn.ind))
        indices.nn <- general_indices(datos.prueba[,variable.predecir], prediccion.nn)
        
        df <- as.data.frame(indices.nn)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfnn <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfnn2 <- render_index_table(df2)
        
        updateData$IndicesM[["nn"]] <<- indices.nn
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        clean_nn(3)
        showNotification(paste0("Error (NN-03) : ",e), duration = 15, type = "error")
      })
    }
  }
 
}
    
## To be copied in the UI
# mod_neural_networks_ui("neural_networks_ui_1")
    
## To be copied in the server
# callModule(mod_neural_networks_server, "neural_networks_ui_1")
 
