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
  
  knn.options <- list(options.run(ns("runKnn")), tags$hr(style = "margin-top: 0px;"),
                      fluidRow(column(numericInput(ns("k.knn"), labelInput("kv"), min = 1,step = 1, value = 7), width = 5),
                               column(selectInput(inputId = ns("kernel.knn"), label = labelInput("selkernel"),selected = "optimal",
                                                  choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                              "triweight", "cos","inv","gaussian")),width = 5)),
                      fluidRow(column(br(),radioSwitch(id = ns("switch_scale_knn"), label = "escal",
                                                       names = c("si", "no")), style = "margin-top: -20px;", width=5),
                               column(width=5, numericInput(ns("distance.knn"), labelInput("distknn"), min = 1,step = 1, value = 2))) )
  
  knn.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                          aceEditor(ns("fieldCodeKnn"), mode = "r", theme = "monokai", value = "", height = "7vh", readOnly = F))
  
  knn.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                   conditionalPanel("input.BoxKnn == 'tabKknPred'",
                                    aceEditor(ns("fieldCodeKnnPred"), mode = "r", theme = "monokai",
                                              value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxKnn == 'tabKknDisp'",
                                    aceEditor(ns("fieldCodeKnnDisp"), mode = "r", theme = "monokai",
                                              value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxKnn == 'tabKknIndex'",
                                    aceEditor(ns("fieldCodeKnnIG"), mode = "r", theme = "monokai",
                                              value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(80,70),
                                       tabs.content = list(knn.options,knn.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(knn.code))
  
  generate.knn.panel <- tabPanel(title = labelInput("generatem"), value = "tabKknModelo",
                                 verbatimTextOutput(ns("txtknn")))
  
  prediccion.knn.panel <- tabPanel(title = labelInput("predm"), value = "tabKknPred",
                                   DT::dataTableOutput(ns("knnPrediTable")))
  
  disp.knn.panel <- tabPanel(title = labelInput("dispersion"), value = "tabKknDisp",
                             echarts4rOutput(ns('plot.knn.disp'), height = "75vh"))
  
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
                             conditionalPanel("input.BoxKnn == 'tabKknModelo'",tabs.options.generate,ns = ns),
                             conditionalPanel("input.BoxKnn != 'tabKknModelo'",tabs.options.Nogenerate,ns = ns)))
  
  tagList(
    page.knn
  )
}

#' KNN Server Function
#'
#' @noRd 
mod_KNN_server <- function(input, output, session,updateData, modelos){
  ns <- session$ns
  
  nombreBase <- "modelo.knn."
  nombreModelo <- "modelo.knn."
  
  return.knn.default.values <- function(){
    updateNumericInput(session, "k.knn", value = 7)
    updateSelectInput(session, "kernel.knn",selected = "optimal")
    updateRadioSwitch(session,"switch_scale_knn","TRUE")
    updateNumericInput(session, "distance.knn", value = 2)
    
    isolate(datos.aprendizaje <- updateData$datos.aprendizaje)
    if(!is.null(datos.aprendizaje)){
      updateNumericInput(session, "k.knn", value = round(sqrt(nrow(datos.aprendizaje))))
    }
    
    # output$txtknn <- renderText(NULL)
    # output$knnPrediTable <- DT::renderDataTable(NULL)
    # output$plot.knn.disp <- renderEcharts4r(NULL)
    # output$indexdfknn <- render_index_table(NULL)
    # output$indexdfknn2 <- render_index_table(NULL)
  }
  
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.knn.default.values()
  })
  
  
  # When the knn model is generated
  observeEvent(input$runKnn, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # Si se tiene los datos entonces :
      knn_full()
    }
  })
  
  
  # Execute model, prediction and indices
  knn_full <- function() {
    tryCatch({
      isolate(datos.aprendizaje <- updateData$datos.aprendizaje)
      isolate(datos.prueba <- updateData$datos.prueba)
      isolate(variable.predecir <- updateData$variable.predecir)
      isolate(scale <- as.logical(input$switch_scale_knn))
      isolate(kernel <- input$kernel.knn)
      isolate(k <- input$k.knn)
      isolate(distance <- input$distance.knn)
      
      nombreModelo <<- paste0(nombreBase, kernel)
      
      #Validacion tamaño del k
      tam <- nrow(datos.aprendizaje)
      k <- ifelse(k >= tam, tam - 2, k)
      
      #Model generate
      modelo.knn <- kkn_model(datos.aprendizaje,variable.predecir, scale, k, kernel, distance)
      updateAceEditor(session, "fieldCodeKnn", value = codeKnn(variable.predecir,scale, k, kernel, distance))
      
      #Prediccion
      prediccion.knn <- kkn_prediction(modelo.knn, datos.prueba)
      updateAceEditor(session, "fieldCodeKnnPred", value = codeKnnPred(nombreModelo))
      
      #Indices
      indices.knn <- general_indices(datos.prueba[,variable.predecir], prediccion.knn)
      updateAceEditor(session, "fieldCodeKnnIG", value = codeKnnIG(variable.predecir))
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$knn[[nombreModelo]] <- list(modelo = modelo.knn, prediccion = prediccion.knn, indices = indices.knn,
                                                  id = kernel))
    }, error = function(e){
      isolate(modelos$knn[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (KNN-00) : ",e), duration = 10, type = "error")
    })
  }
  
  #Update model tab
  output$txtknn <- renderPrint({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        modelos.knn <- modelos$knn[[nombreModelo]]$modelo
        print(modelos.knn)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (KNN-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  # Update prediction tab
  output$knnPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        prediccion.knn <- modelos$knn[[nombreModelo]]$prediccion
        isolate(datos.prueba <- updateData$datos.prueba)
        isolate(real.val <- datos.prueba[updateData$variable.predecir])
        tb_predic(real.val, prediccion.knn, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (KNN-02) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update Dispersion Tab
  output$plot.knn.disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        prediccion.knn <- modelos$knn[[nombreModelo]]$prediccion
        isolate(datos.prueba <- updateData$datos.prueba)
        isolate(variable.predecir <- updateData$variable.predecir)
        isolate(kernel <- input$kernel.knn)
        idioma <- updateData$idioma
        
        codigo <- disp_models(nombreModelo, paste0(tr("knn", idioma),"-",kernel), variable.predecir)
        updateAceEditor(session, "fieldCodeKnnDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", idioma),
          tr("realValue", idioma),
          tr("pred", idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.knn,
                             paste0(tr("knn", idioma),"-",kernel),titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (KNN-03) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  
  #Update Indices tab
  output$indexdfknn <- renderTable({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        idioma <- updateData$idioma
        indices.knn <- modelos$knn[[nombreModelo]]$indices
        df <- as.data.frame(indices.knn)
        colnames(df) <- c(tr("RMSE", idioma), tr("MAE", idioma), tr("ER", idioma), tr("correlacion", idioma))
        df
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (KNN-04) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',  digits = 5,align = 'c')
  
  
  
  output$indexdfknn2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
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
      showNotification(paste0("Error (KNN-05) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',  digits = 5,align = 'c')
  
}

## To be copied in the UI
# mod_KNN_ui("KNN_ui_1")

## To be copied in the server
# callModule(mod_KNN_server, "KNN_ui_1")

