#' KNN UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_KNN_ui <- function(id){
  ns <- NS(id)
  
  knn.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                               column(width = 2,br(),actionButton(ns("runKnn"), label = labelInput("ejecutar"), icon = icon("play")))),
                      hr(),
                      fluidRow(column(numericInput(ns("kmax.knn"), labelInput("kmax"), min = 1,step = 1, value = 7), width = 6),
                               column(selectInput(inputId = ns("kernel.knn"), label = labelInput("selkernel"),selected = "optimal",
                                                  choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                              "triweight", "cos","inv","gaussian")),width = 6)),
                      fluidRow(column(br(),switchInput(inputId = ns("switch.scale.knn"), onStatus = "success", offStatus = "danger", value = T,
                                                       label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=6),
                               column(width=6, numericInput(ns("distance.knn"), labelInput("distknn"), min = 1,step = 1, value = 2))) )
  
  knn.code <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxKnn == 'tabKknModelo'",
                                    aceEditor(ns("fieldCodeKnn"), mode = "r", theme = "monokai", value = "", height = "4vh", readOnly = F),ns = ns),
                   conditionalPanel("input.BoxKnn == 'tabKknPred'",
                                    aceEditor(ns("fieldCodeKnnPred"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxKnn == 'tabKknDisp'",
                                    aceEditor(ns("fieldCodeKnnDisp"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxKnn == 'tabKknIndex'",
                                    aceEditor(ns("fieldCodeKnnIG"), mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled"),ns = ns))
  
  tabs.knn <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                          tabs.content = list(knn.options, knn.code))
  
  generate.knn.panel <- tabPanel(title = labelInput("generatem"), value = "tabKknModelo",
                                 verbatimTextOutput(ns("txtknn")))
  
  prediccion.knn.panel <- tabPanel(title = labelInput("predm"), value = "tabKknPred",
                                   DT::dataTableOutput(ns("knnPrediTable")))
  
  disp.knn.panel <- tabPanel(title = labelInput("dispersion"), value = "tabKknDisp",
                             plotOutput(ns('plot.knn.disp'), height = "55vh"))
  
  general.index.knn.panel <- tabPanel(title = labelInput("indices"), value = "tabKknIndex",
                                      br(),
                                      fluidRow(tableOutput(ns('indexdfknn'))),
                                      br(),
                                      fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                      br(),
                                      fluidRow(tableOutput(ns('indexdfknn2'))))
  
  page.knn <- tabItem(tabName = "knn",
                      tabBox(id = ns("BoxKnn"), width = NULL, height ="80%",
                             generate.knn.panel,
                             prediccion.knn.panel,
                             disp.knn.panel,
                             general.index.knn.panel,
                             tabs.knn))
  
  tagList(
    page.knn
  )
}
    
#' KNN Server Function
#'
#' @noRd 
mod_KNN_server <- function(input, output, session,updateData, updatePlot){
  ns <- session$ns
  
  return.knn.default.values <- function(){
    updateSelectInput(session, "kernel.knn",selected = "optimal")
    updateSwitchInput(session,"switch.scale.knn", value = T)
    updateNumericInput(session, "distance.knn", value = 2)
    
    knn.args.default <<- TRUE
    
    output$txtknn <- renderText(NULL)
    output$knnPrediTable <- DT::renderDataTable(NULL)
    output$plot.knn.disp <- renderPlot(NULL)
    output$indexdfknn <- render_index_table(NULL)
    output$indexdfknn2 <- render_index_table(NULL)
  }
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.knn.default.values()
  })
  
 
  # When the knn model is generated
  observeEvent(input$runKnn, {
    if (validate_data()) { # Si se tiene los datos entonces :
      default_codigo_knn()
      knn_full()
    }
  })
  
  # When the user changes the parameters
  # observeEvent(c(input$switch.scale.knn, input$kmax.knn, input$kernel.knn, input$distance.knn), {
  #   if (validate_data(print = FALSE) & knn.stop.excu) {
  #     default_codigo_knn()
  #   }else{
  #     knn.stop.excu <<- TRUE
  #   }
  # })
  
  # Upgrade code fields to default version
  default_codigo_knn <- function() {
    if(!is.null(datos.aprendizaje) & knn.args.default){
      k.value <- round(sqrt(nrow(datos.aprendizaje)))
      updateNumericInput(session,"kmax.knn",value = k.value)
      knn.args.default <<- FALSE
    }else{
      k.value <- input$kmax.knn
    }
    
    # Se acualiza el codigo del modelo
    codigo <- kkn_model(variable.pred = variable.predecir,
                        scale = input$switch.scale.knn,
                        kmax = k.value,
                        kernel = input$kernel.knn,
                        model.var = paste0("modelo.knn.", input$kernel.knn),
                        distance = input$distance.knn)
    
    updateAceEditor(session, "fieldCodeKnn", value = codigo)
    cod.knn.modelo <<- codigo
    
    # Se genera el codigo de la prediccion
    codigo <- kkn_prediction(variable.pred = variable.predecir,
                             model.var = paste0("modelo.knn.", input$kernel.knn), 
                             pred.var  = paste0("prediccion.knn.", input$kernel.knn))
    
    updateAceEditor(session, "fieldCodeKnnPred", value = codigo)
    cod.knn.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models(paste0("prediccion.knn.", input$kernel.knn), translate("knnl"), variable.predecir)
    updateAceEditor(session, "fieldCodeKnnDisp", value = codigo)
    
    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeKnnIG", value = codigo)
    cod.knn.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_knn <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        exe("modelo.knn.",input$kernel.knn," <<- NULL")
        output$txtknn <- renderPrint(invisible(""))
        #remove_report_elem(paste0("modelo.knn.",input$kernel.knn))
      }, {
        exe("prediccion.knn.",input$kernel.knn," <<- NULL")
        #remove_report_elem(paste0("pred.knn.",input$kernel.knn))
        output$knnPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("indices.knn.",input$kernel.knn," <<- NULL")
        #remove_report_elem(paste0("ind.knn.",input$kernel.knn))
      })
    }
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_knn <- function(){
    tryCatch({ # Se corren los codigo
      isolate(kernel <- input$kernel.knn)
      output$plot.knn.disp <- renderPlot(exe(input$fieldCodeKnnDisp))
      #insert_report(paste0("disp.knn.",kernel), paste0("Dispersi\u00F3n del Modelo KNN (",kernel,")"), codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_knn(2)
      showNotification(paste0("Error (KNN-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  knn_full <- function() {
    execute_knn()
    execute_knn_pred()
    execute_knn_ind()
  }
  
  # Generates the model
  execute_knn <- function() {
    tryCatch({
      exe(cod.knn.modelo)
      isolate(kernel <- input$kernel.knn)
      updateAceEditor(session, "fieldCodeKnn", value = cod.knn.modelo)
      output$txtknn <- renderPrint(exe("modelo.knn.",kernel))
      #insert_report(paste0("modelo.knn.",kernel), paste0("Generaci\u00F3n del Modelo KNN (",kernel,")"),cod.knn.modelo,"\nmodelo.knn.", kernel)
      
      #nombres.modelos <<- c(nombres.modelos, paste0("modelo.knn.",kernel))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_knn(1)
      showNotification(paste0("Error (KNN-01) : ", e), duration = 15, type = "error")
    }
    )
  }
  
  # Generate the prediction
  execute_knn_pred <- function() {
    tryCatch({ # Se corren los codigo
      exe(cod.knn.pred)
      isolate(kernel <- input$kernel.knn)
      
      # Cambia la tabla con la prediccion de knn
      output$knnPrediTable <- DT::renderDataTable(tb_predic(real.val, exe("prediccion.knn.",kernel)), server = FALSE)
      # insert_report(paste0("pred.knn.",kernel), paste0("Predicci\u00F3n del Modelo KNN (",kernel,")"), 
      #               cod.knn.pred,"\nkt(head(tb_predic(real.val, prediccion.knn.",kernel,")$x$data[,-1]))", interpretation = FALSE)
      
      plot_disp_knn()
      #nombres.modelos <<- c(nombres.modelos, paste0("prediccion.knn.",kernel))
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_knn(2)
      showNotification(paste0("Error (KNN-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_knn_ind <- function(){
    if(exists(paste0("prediccion.knn.",input$kernel.knn))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.knn.ind))
        isolate(kernel <- input$kernel.knn)
        
        indices.knn <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.knn.",kernel))
        #eval(parse(text = paste0("indices.knn.",kernel, "<<- indices.knn")))
        
        # insert_report(paste0("ind.knn.",kernel), paste0("\u00CDndices del Modelo KNN (",kernel,")"),
        #               cod.knn.ind, "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'] ,prediccion.knn.",kernel,"))\n",
        #               "indices.knn <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.knn.",kernel,")\n",
        #               "IndicesM[['knnl-",kernel,"']] <- indices.knn")
        
        df <- as.data.frame(indices.knn)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfknn <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfknn2 <- render_index_table(df2)
        
        #nombres.modelos <<- c(nombres.modelos, paste0("indices.knn.",kernel))
        updateData$IndicesM[[paste0("knnl-",kernel)]] <<- indices.knn
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_knn(3)
        showNotification(paste0("Error (KNN-03) : ",e), duration = 15, type = "error")
      })
    }
  }
}
    
## To be copied in the UI
# mod_KNN_ui("KNN_ui_1")
    
## To be copied in the server
# callModule(mod_KNN_server, "KNN_ui_1")
 
