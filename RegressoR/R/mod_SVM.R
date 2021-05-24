#' SVM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SVM_ui <- function(id){
  
  ns <- NS(id)
  
  svm.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                               column(width = 2,br(),actionButton(ns("runSvm"), label = labelInput("ejecutar"), icon = icon("play")))),
                      hr(),
                      conditionalPanel("input.BoxSvm != 'tabSvmPlot'",
                                       fluidRow(column(br(),switchInput(inputId = ns("switch.scale.svm"), onStatus = "success", offStatus = "danger", value = T,
                                                                        label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width = 6),
                                                column(selectInput(inputId = ns("kernel.svm"), label = labelInput("selkernel"), selected = "radial",
                                                                   choices =  c("linear", "polynomial", "radial", "sigmoid")), width=6)), ns = ns))
  
  svm.code <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxSvm == 'tabSvmModelo'",
                                    aceEditor(ns("fieldCodeSvm"), mode = "r", theme = "monokai", value = "", height = "3vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                   conditionalPanel("input.BoxSvm == 'tabSvmDisp'",
                                    aceEditor(ns("fieldCodeSvmDisp"), mode = "r", theme = "monokai",
                                              value = "", height = "6vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                   conditionalPanel("input.BoxSvm == 'tabSvmPred'",
                                    aceEditor(ns("fieldCodeSvmPred"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                   conditionalPanel("input.BoxSvm == 'tabSvmIndex'",
                                    aceEditor(ns("fieldCodeSvmIG"), mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled"), ns = ns))
  
  tabs.svm <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(60, 95),
                          tabs.content = list(svm.options, svm.code))
  
  generate.svm.panel <- tabPanel(title = labelInput("generatem"), value = "tabSvmModelo",
                                 verbatimTextOutput(ns("txtSvm")))
  
  disp.svm.panel <- tabPanel(title = labelInput("dispersion"), value = "tabSvmDisp",
                             plotOutput(ns('plot.svm.disp'), height = "55vh"))
  
  prediction.svm.panel <- tabPanel(title = labelInput("predm"), value = "tabSvmPred",
                                   DT::dataTableOutput(ns("svmPrediTable")))
  
  general.index.svm.panel <- tabPanel(title = labelInput("indices"), value = "tabSvmIndex",
                                      br(),
                                      fluidRow(tableOutput(ns('indexdfsvm'))),
                                      br(),
                                      fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                      br(),
                                      fluidRow(tableOutput(ns('indexdfsvm2'))))
  
  page.svm <- tabItem(tabName = "svm",
                      tabBox(id = ns("BoxSvm"), width = NULL, height ="80%",
                             generate.svm.panel,
                             prediction.svm.panel,
                             disp.svm.panel,
                             general.index.svm.panel,
                             tabs.svm))
  
  
  tagList(
    page.svm
  )
}
    
#' SVM Server Function
#'
#' @noRd 
mod_SVM_server <- function(input, output, session,updateData, updatePlot){
  ns <- session$ns
  
  return.svm.default.values <- function(){
    output$txtSvm <- renderText(NULL)
    output$svmPrediTable <- DT::renderDataTable(NULL)
    output$plot.svm.disp <- renderPlot(NULL)
    output$indexdfsvm <- render_index_table(NULL)
    output$indexdfsvm2 <- render_index_table(NULL)
  }
  
  # When the knn model is generated
  observeEvent(input$runSvm, {
    if (validate_data()) { # Si se tiene los datos entonces :
      default_codigo_svm()
      svm_full()
    }
  })
  
  # When the user changes the parameters
  # observeEvent(c(input$switch.scale.svm, input$kernel.svm), {
  #   if (validate_data(print = FALSE)){
  #     default_codigo_svm()
  #   }
  # })
  
  # Upgrade code fields to default version
  default_codigo_svm <- function() {
    # Se acualiza el codigo del modelo
    codigo <- svm_model(variable.pred = variable.predecir,
                        model.var = paste0("modelo.svm.",input$kernel.svm),
                        scale = input$switch.scale.svm,
                        kernel = input$kernel.svm)
    
    updateAceEditor(session, "fieldCodeSvm", value = codigo)
    cod.svm.modelo <<- codigo
    
    # Se genera el codigo de la prediccion
    codigo <- svm_prediction(variable.pred = variable.predecir,
                             model.var = paste0("modelo.svm.",input$kernel.svm),
                             pred.var = paste0("prediccion.svm.",input$kernel.svm))
    updateAceEditor(session, "fieldCodeSvmPred", value = codigo)
    cod.svm.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models(paste0("prediccion.svm.",input$kernel.svm), translate("svml"), variable.predecir)
    updateAceEditor(session, "fieldCodeSvmDisp", value = codigo)
    
    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeSvmIG", value = codigo)
    cod.svm.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_svm <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        exe("modelo.svm.",input$kernel.svm,"<<- NULL")
        output$txtSvm <- renderPrint(invisible(""))
        #remove_report_elem(paste0("modelo.svm.",input$kernel.svm))
      }, {
        exe("prediccion.svm.",input$kernel.svm,"<<- NULL")
        #remove_report_elem(paste0("pred.svm.",input$kernel.svm))
        output$svmPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("indices.svm.",input$kernel.svm,"<<- NULL")
        #remove_report_elem(paste0("ind.svm.",input$kernel.svm))
      })
    }
  }
  
  # Execute model, prediction and indices
  svm_full <- function() {
    execute_svm()
    execute_svm_pred()
    execute_svm_ind()
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_svm <- function(){
    tryCatch({ # Se corren los codigo
      isolate(kernel <- input$kernel.svm)
      output$plot.svm.disp <- renderPlot(exe(input$fieldCodeSvmDisp))
      #insert_report(paste0("disp.svm.", kernel), paste0("Dispersi\u00F3n del Modelo SVM (",kernel,")"), codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(2)
      showNotification(paste0("Error (SVM-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the model
  execute_svm <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.svm.modelo))
      isolate(kernel <- input$kernel.svm)
      output$txtSvm <- renderPrint(exe("print(modelo.svm.",kernel,")"))
      updateAceEditor(session, "fieldCodeSvm", value = cod.svm.modelo)
      
      #insert_report(paste0("modelo.svm.",kernel), paste0("Generaci\u00F3n del Modelo SVM (",kernel,")"), cod.svm.modelo, "\nmodelo.svm.", kernel)
      
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.svm.", kernel))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(1)
      showNotification(paste0("Error (SVM-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_svm_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.svm.pred))
      isolate(kernel <- input$kernel.svm)
      
      # Cambia la tabla con la prediccion de knn
      output$svmPrediTable <- DT::renderDataTable(exe("tb_predic(real.val, prediccion.svm.",kernel,")"),server = FALSE)
      # insert_report(paste0("pred.svm.",input$kernel.svm), paste0("Predicci\u00F3n del Modelo SVM (",kernel,")"), 
      #               cod.svm.pred,"\nkt(head(tb_predic(real.val, prediccion.svm.",kernel,")$x$data[,-1]))",interpretation = FALSE)
      
      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.svm.",kernel))
      
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(2)
      showNotification(paste0("Error (SVM-02) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_svm_ind <- function(){
    if(exists(paste0("prediccion.svm.",input$kernel.svm))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.svm.ind))
        isolate(kernel <- input$kernel.svm)
        
        indices.svm <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.svm.",kernel))
        #eval(parse(text =paste0("indices.svm.",kernel, "<<- indices.svm")))
        
        # insert_report(paste0("ind.svm.",kernel), paste0("\u00CDndices Generales del modelo SVM (",kernel,")"),
        #               cod.svm.ind, "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.svm.",kernel,"))\n",
        #               "indices.svm <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.svm.",kernel,")\n",
        #               "IndicesM[['svml-",kernel,"']] <- indices.svm")
        
        df <- as.data.frame(indices.svm)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfsvm <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfsvm2 <- render_index_table(df2)
        
        plot_disp_svm()
        
        #nombres.modelos <<- c(nombres.modelos, paste0("indices.svm.",kernel))
        updateData$IndicesM[[paste0("svml-",kernel)]] <<- indices.svm
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_svm(3)
        showNotification(paste0("Error (SVM-03) : ",e), duration = 15, type = "error")
      })
    }
  }
 
}
    
## To be copied in the UI
# mod_SVM_ui("SVM_ui_1")
    
## To be copied in the server
# callModule(mod_SVM_server, "SVM_ui_1")
 
