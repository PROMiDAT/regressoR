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
                      fluidRow(column(radioSwitch(id = ns("switch_scale_knn"), label = "escal",
                                                       names = c("si", "no")), width=5),
                               column(width=5, numericInput(ns("distance.knn"), labelInput("distknn"), min = 1,step = 1, value = 2))) )
  
  
  tabs.options.generate <- tabsOptions(widths = c(100), heights = c(70),
                                       tabs.content = list(knn.options))
  
  
  tabs.options <- list(conditionalPanel("input.BoxKnn == 'tabKknModelo'",tabs.options.generate,ns = ns))
  
  generate.knn.panel <- tabPanel(title = labelInput("generatem"), value = "tabKknModelo",
                                 withLoader(verbatimTextOutput(ns("txtknn")),type = "html", loader = "loader4"))
  
  prediccion.knn.panel <- tabPanel(title = labelInput("predm"), value = "tabKknPred",
                                   withLoader(DT::dataTableOutput(ns("knnPrediTable")),type = "html", loader = "loader4"))
  
  disp.knn.panel <- tabPanel(title = labelInput("dispersion"), value = "tabKknDisp",
                             withLoader(echarts4rOutput(ns('plot_knn_disp'),height = "75vh"),type = "html", loader = "loader4"))
  
  general.index.knn.panel <- tabPanel(title = labelInput("indices"), value = "tabKknIndex",
                                      br(),
                                      fluidRow(withLoader(tableOutput(ns('indexdfknn')),type = "html", loader = "loader4")),
                                      br(),
                                      fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                      br(),
                                      fluidRow(withLoader(tableOutput(ns('indexdfknn2')),type = "html", loader = "loader4")))
  rmse.knn.panel <- tabPanel(title = labelInput("RMSE"), value = "tabKknRMSE",
                             withLoader(echarts4rOutput(ns('plot_knn_rmse'),height = "75vh"),type = "html", loader = "loader4"))
  
  page.knn <- tabItem(tabName = "knn",
                      tabBoxPrmdt(id = ns("BoxKnn"), opciones = tabs.options,
                             generate.knn.panel,
                             prediccion.knn.panel,
                             rmse.knn.panel,
                             disp.knn.panel,
                             general.index.knn.panel))
  
  tagList(
    page.knn
  )
}

#' KNN Server Function
#'
#' @noRd 
mod_KNN_server <- function(input, output, session,updateData, modelos, codedioma){
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
    
    nombreModelo <- "modelo.knn."
  }
  
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.knn.default.values()
  })
  
  
  #Update model tab
  output$txtknn <- renderPrint({
    input$runKnn
    tryCatch({
      codigo.knn()
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba      <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        scale  <- as.logical(input$switch_scale_knn)
        kernel <- input$kernel.knn
        k        <- input$k.knn
        distance <- input$distance.knn
      })
      
      nombreModelo <<- paste0(nombreBase, kernel)
      
      #Validacion tamaño del k
      tam <- nrow(datos.aprendizaje)
      if(k >= tam){
        k <- tam - 2
        updateNumericInput(session, "k.knn", value = tam - 2)
      }
      var    <- as.formula(paste0(variable.predecir, "~."))
      
      #Model generate
      modelo.knn <- traineR::train.knn(var, data = datos.aprendizaje, scale = as.logical(scale), 
                                      kernel = kernel, ks = k, distance = distance )
      #Prediccion
      prediccion.knn <- predict(modelo.knn, datos.prueba)

      #Indices
      indices.knn <- general_indices(datos.prueba[,variable.predecir], prediccion.knn$prediction)

      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$knn[[nombreModelo]] <- list(modelo = modelo.knn, prediccion = prediccion.knn$prediction, 
                                                  indices = indices.knn, id = kernel))
      #Cambiamos la forma en que va aparecer el call
      modelo.knn$call$formula  <- var
      modelo.knn$call$ks       <- k
      modelo.knn$call$kernel   <- kernel
      modelo.knn$call$scale    <- scale
      modelo.knn$call$distance <- distance
      
      print(modelo.knn)
    }, error = function(e){
      isolate(modelos$knn[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (KNN-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  # Update prediction tab
  output$knnPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        prediccion.knn <- modelos$knn[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.knn, updateData$decimals, codedioma$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (KNN-02) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  # Update rmse tab
  output$plot_knn_rmse <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        isolate({
          train <- updateData$datos.aprendizaje
          test  <- updateData$datos.prueba
          variable.pred <- updateData$variable.predecir
          scale    <- as.logical(input$switch_scale_knn)
          kernel   <- input$kernel.knn
          distance <- input$distance.knn
        })
        k_value <- input$k.knn
        df_plot <- rmse_k_values(train = train, k = c(1:k_value),
                                 test  = test, variable.pred = variable.pred, 
                                 scale = scale, kernel = kernel, distance = distance)
        plot_RMSEK(datos = df_plot , modelo.knn = modelos$knn[[nombreModelo]]$modelo)
        
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (KNN-03) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  # Update Dispersion Tab
  output$plot_knn_disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        prediccion.knn <- modelos$knn[[nombreModelo]]$prediccion
        isolate({
          datos.prueba      <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
          kernel            <- input$kernel.knn
        })
        idioma <- codedioma$idioma
        
        codigo <- disp_models("modelo.knn", paste0(tr("knn", idioma),"-",kernel), variable.predecir)
        cod    <- paste0("### docdisp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
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
        idioma <- codedioma$idioma
        indices.knn <- modelos$knn[[nombreModelo]]$indices
        tabla.indicesPrecision(indices.knn, updateData$decimals, idioma)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (KNN-04) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',align = 'c')
  
  
  
  output$indexdfknn2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        idioma <- codedioma$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(summary_indices(updateData$datos.prueba[,updateData$variable.predecir]),
                              decimals, 
                              idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (KNN-05) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',align = 'c')
  
  # Execute model, prediction and indices
  codigo.knn <- function() {
    tryCatch({
      
      isolate({
        variable.predecir <- updateData$variable.predecir
        scale  <- as.logical(input$switch_scale_knn)
        kernel <- input$kernel.knn
        k      <- input$k.knn
        distance <- input$distance.knn
      })
      #Model generate
      codigo <- codeKnn(variable.predecir,scale, k, kernel, distance)
      cod    <- paste0("### KNN\n", codigo)
      #Prediccion
      codigo <- codeKnnPred()
      cod    <- paste0(cod, codigo)
      #Indices
      codigo <- codeKnnIG(variable.predecir)
      cod    <- paste0(cod, codigo)
      isolate(codedioma$code <- append(codedioma$code, cod))
      
    }, error = function(e){
      showNotification(paste0("Error (KNN-00) : ",e), duration = 10, type = "error")
    })
  }
  
}

## To be copied in the UI
# mod_KNN_ui("KNN_ui_1")

## To be copied in the server
# callModule(mod_KNN_server, "KNN_ui_1")

