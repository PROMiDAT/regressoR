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
  
  svm.options <- list(options.run(ns("runSvm")), tags$hr(style = "margin-top: 0px;"),
                      conditionalPanel("input.BoxSvm != 'tabSvmPlot'",
                                       fluidRow(column(br(),switchInput(inputId = ns("switch.scale.svm"), onStatus = "success", offStatus = "danger", value = T,
                                                                        label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width = 6),
                                                column(selectInput(inputId = ns("kernel.svm"), label = labelInput("selkernel"), selected = "radial",
                                                                   choices =  c("linear", "polynomial", "radial", "sigmoid")), width=6)), ns = ns))
  
  svm.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                          aceEditor(ns("fieldCodeSvm"), mode = "r", theme = "monokai", value = "", height = "3vh", readOnly = F, autoComplete = "enabled"))
  
  
  svm.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                   conditionalPanel("input.BoxSvm == 'tabSvmDisp'",
                                    aceEditor(ns("fieldCodeSvmDisp"), mode = "r", theme = "monokai",
                                              value = "", height = "6vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                   conditionalPanel("input.BoxSvm == 'tabSvmPred'",
                                    aceEditor(ns("fieldCodeSvmPred"), mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled"), ns = ns),
                   conditionalPanel("input.BoxSvm == 'tabSvmIndex'",
                                    aceEditor(ns("fieldCodeSvmIG"), mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled"), ns = ns))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(80,95),
                                       tabs.content = list(svm.options,svm.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(95),
                                         tabs.content = list(svm.code))
  
  generate.svm.panel <- tabPanel(title = labelInput("generatem"), value = "tabSvmModelo",
                                 verbatimTextOutput(ns("txtSvm")))
  
  disp.svm.panel <- tabPanel(title = labelInput("dispersion"), value = "tabSvmDisp",
                             echarts4rOutput(ns('plot.svm.disp'), height = "75vh"))
  
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
                             conditionalPanel("input.BoxSvm == 'tabSvmModelo'",tabs.options.generate,ns = ns),
                             conditionalPanel("input.BoxSvm != 'tabSvmModelo'",tabs.options.Nogenerate,ns = ns)))
  
  
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
    updateSwitchInput(session,"switch.scale.svm",value = T)
    updateSelectInput(session,"kernel.svm",selected = "radial")
    
    output$txtSvm <- renderText(NULL)
    output$svmPrediTable <- DT::renderDataTable(NULL)
    output$plot.svm.disp <- renderEcharts4r(NULL)
    output$indexdfsvm <- render_index_table(NULL)
    output$indexdfsvm2 <- render_index_table(NULL)
  }
  
  observeEvent(updateData$idioma,{
    model.var = paste0("modelo.svm.",input$kernel.svm)
    if(exists(model.var)){
      execute_svm_ind()
      plot_disp_svm()
    }
  })
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.svm.default.values()
  })
  
  # When the knn model is generated
  observeEvent(input$runSvm, {
    if (validate_data()) { # Si se tiene los datos entonces :
      default_codigo_svm()
      svm_full()
    }
  })

  
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
      }, {
        exe("prediccion.svm.",input$kernel.svm,"<<- NULL")
        output$svmPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("indices.svm.",input$kernel.svm,"<<- NULL")
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
      
      titulos <- c(
        tr("predvsreal", updateData$idioma),
        tr("realValue", updateData$idioma),
        tr("pred", updateData$idioma)
      )
      
      output$plot.svm.disp <- renderEcharts4r(plot_real_prediction(datos.prueba[variable.predecir],
                                                                  exe(paste0("prediccion.svm.",input$kernel.svm)),
                                                                  translate("svml"),titulos))
      
      codigo <- disp_models(paste0("prediccion.svm.",input$kernel.svm), translate("svml"), variable.predecir)
      updateAceEditor(session, "fieldCodeSvmDisp", value = codigo)
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
      
      #nombres.modelos <<- c(nombres.modelos, paste0("modelo.svm.", kernel))
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
      output$svmPrediTable <- DT::renderDataTable(tb_predic(real.val,exe(paste0("prediccion.svm.",kernel))), server = FALSE)
      
      #nombres.modelos <<- c(nombres.modelos, paste0("prediccion.svm.",kernel))
      
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(2)
      showNotification(paste0("Error (SVM-02) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_svm_ind <- function(){
    var.prediction.name <- paste0("prediccion.svm.",input$kernel.svm)
    if(exists(var.prediction.name)){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.svm.ind))
        isolate(kernel <- input$kernel.svm)
        
        indices.svm <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.svm.",kernel))
        #eval(parse(text =paste0("indices.svm.",kernel, "<<- indices.svm")))
        
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
 
