#' linear_regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_linear_regression_ui <- function(id){
  ns <- NS(id)
  
  rl.code  <- list(
                   conditionalPanel("input.BoxRl == 'tabRlModelo'",
                                    options.run(ns("runRl")), tags$hr(style = "margin-top: 0px;"),
                                    aceEditor(ns("fieldCodeRl"), mode = "r", theme = "monokai",
                                              value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRl != 'tabRlModelo'", 
                                    h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),ns = ns),
                   conditionalPanel("input.BoxRl == 'tabRlCoef'",
                                    aceEditor(ns("fieldCodeRlCoef"), mode = "r", theme = "monokai",
                                              value = "", height = "10vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRl == 'tabRlPred'",
                                    aceEditor(ns("fieldCodeRlPred"), mode = "r", theme = "monokai",
                                              value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRl == 'tabRlDisp'",
                                    aceEditor(ns("fieldCodeRlDisp"), mode = "r", theme = "monokai",
                                              value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                   conditionalPanel("input.BoxRl == 'tabRlIndex'",
                                    aceEditor(ns("fieldCodeRlIG"), mode = "r", theme = "monokai",
                                              value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns))
  
  tabs.rl  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                          tabs.content = list(rl.code))
  
  generate.rl.panel <- tabPanel(title = labelInput("generatem"),value = "tabRlModelo",
                                verbatimTextOutput(ns("txtRl")))
  
  coefficients.rl.panel <- tabPanel(title = labelInput("coeff"), value = "tabRlCoef",
                                    DT::dataTableOutput(ns("rlCoefTable")))
  
  prediccion.rl.panel <- tabPanel(title = labelInput("predm"), value = "tabRlPred",
                                  DT::dataTableOutput(ns("rlPrediTable")))
  
  disp.rl.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRlDisp",
                            echarts4rOutput(ns('plot.rl.disp'), height = "75vh"))
  
  rl.general.index.panel <- tabPanel(title = labelInput("indices"), value = "tabRlIndex",
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfrl'))),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(tableOutput(ns('indexdfrl2'))))
  
  
  page.rl <- tabItem(tabName = "rl",
                     tabBox(id = ns("BoxRl"), width = NULL, height ="80%",
                            generate.rl.panel,
                            coefficients.rl.panel,
                            prediccion.rl.panel,
                            disp.rl.panel,
                            rl.general.index.panel,
                            tabs.rl))
  
  tagList(
    page.rl
  )
}
    
#' linear_regression Server Function
#'
#' @noRd 
mod_linear_regression_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  
  nombreModelo <- "modelo.rl"
  df.rl <- NULL
  r2 <- NULL
  
  return.rl.default.values <- function(){
    #output$txtRl <- renderText(NULL)
    # output$rlCoefTable <- DT::renderDataTable(NULL)
    # output$rlPrediTable <- DT::renderDataTable(NULL)
    # output$plot.rl.disp <- renderEcharts4r(NULL)
    # output$indexdfrl <- renderTable(NULL)
    # output$indexdfrl2 <- renderTable(NULL)
    df.rl <- NULL
    r2 <- NULL
  }
  

  observeEvent(updateData$datos.aprendizaje,{
    #Change to default values
    return.rl.default.values()
  })

  # observeEvent(input$runRl, {
  #   print("Hello")
  #   isolate(lenguage <- updateData$idioma)
  #   if (validate_data(isolate(updateData), idioma = lenguage)) { # Si se tiene los datos entonces :
  #     rl_full()
  #   }
  # })
  
  # Execute model, prediction and indices
  rl_full <- function(){
    isolate(datos.aprendizaje <- updateData$datos.aprendizaje)
    isolate(datos.prueba <- updateData$datos.prueba)
    isolate(variable.predecir <- updateData$variable.predecir)
    
    #Model generate
    modelo.rl <- rl_model(datos.aprendizaje,variable.predecir)
    
    #Coefficients
    model.information <- rl_coeff(modelo.rl)
    df.rl <<- model.information$df.rl
    r2 <<- model.information$r2
    #Prediccion
    prediccion.rl <- rl_prediction(modelo.rl, datos.prueba)
    #Indices
    indices.rl <- general_indices(datos.prueba[,variable.predecir], prediccion.rl)
    
    modelos$rl[[nombreModelo]] <- list(modelo = modelo.rl, prediccion = prediccion.rl, indices = indices.rl)
  }
  
  
  #Update model tab
  output$txtRl <- renderPrint({
    input$runRl
    tryCatch({
      isolate(datos.aprendizaje <- updateData$datos.aprendizaje)
      isolate(datos.prueba <- updateData$datos.prueba)
      isolate(variable.predecir <- updateData$variable.predecir)
      
      #Model generate
      modelo.rl <- rl_model(datos.aprendizaje,variable.predecir)
      
      #Coefficients
      model.information <- rl_coeff(modelo.rl)
      df.rl <<- model.information$df.rl
      r2 <<- model.information$r2
      #Prediccion
      prediccion.rl <- rl_prediction(modelo.rl, datos.prueba)
      #Indices
      indices.rl <- general_indices(datos.prueba[,variable.predecir], prediccion.rl)
      
      modelos$rl[[nombreModelo]] <- list(modelo = modelo.rl, prediccion = prediccion.rl, indices = indices.rl)
      
      #rl_full()
      #modelo.rl <- modelos$rl[[nombreModelo]]$modelo
      if(!is.null(modelo.rl)){
        isolate(variable.predecir <- updateData$variable.predecir)
        #updateAceEditor(session, "fieldCodeRl", value = codeRl(variable.predecir))
        #return(summary(modelo.rl))
      }
      # else{NULL}
      return("Hola-B")
    }, error = function(e){
      showNotification(paste0("Error (RL-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  #Update Coefficients tab
  #Necesita observeEvent porque render_table_data no es reactivo
  observeEvent(c(modelos$rl,updateData$idioma),{
    tryCatch({
      if(!is.null(df.rl)){
        updateAceEditor(session, "fieldCodeRlCoef", value = codeRlCoef())
        output$rlCoefTable <- render_table_data(df.rl[,c(1,4)], server = FALSE, language = updateData$idioma)
      }else{NULL}
    },
    error = function(e) {
      showNotification(paste0("Error (RL-02) : ", e), duration = 10, type = "error")
    })
  },ignoreInit = T)

  
  
  # Update prediction tab
  output$rlPrediTable <- DT::renderDataTable({
    input$runRl
    print("LSACD")
    tryCatch({
      prediccion.rl <- modelos$rl[[nombreModelo]]$prediccion
      if(!is.null(prediccion.rl)){
        isolate(datos.prueba <- updateData$datos.prueba)
        isolate(real.val <- datos.prueba[updateData$variable.predecir])
        
        updateAceEditor(session, "fieldCodeRlPred", value = codeRlPred())
        tb_predic(real.val, prediccion.rl, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RL-03) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update dispersion tab
  output$plot.rl.disp <- renderEcharts4r({
    tryCatch({
      prediccion.rl <- modelos$rl[[nombreModelo]]$prediccion
      if(!is.null(prediccion.rl)){
        isolate(datos.prueba <- updateData$datos.prueba)
        isolate(variable.predecir <- updateData$variable.predecir)
        codigo <- disp_models("prediccion.rl", tr("rll",updateData$idioma), variable.predecir)
        updateAceEditor(session, "fieldCodeRlDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", updateData$idioma),
          tr("realValue", updateData$idioma),
          tr("pred", updateData$idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.rl,tr("rll",updateData$idioma),titulos)
      }
      else{NULL}
    },
    error = function(e) {
      showNotification(paste0("Error (RL-04): ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  observeEvent(c(modelos$rl,updateData$idioma),{
    
    idioma <- updateData$idioma
    
    output$indexdfrl <- render_index_table({
      tryCatch({
        indices.rl <- modelos$rl[[nombreModelo]]$indices
        if(!is.null(indices.rl)){
          updateAceEditor(session, "fieldCodeRlIG", value = codeRlIG(variable.predecir))
          df <- cbind(as.data.frame(indices.rl), r2)
          df <- df[,c(1,2,3,5,4)]
          colnames(df) <- c(tr("RMSE",idioma), tr("MAE",idioma),
                            tr("ER",idioma), tr("R2",idioma),
                            tr("correlacion", idioma))
          df
        }
        else{NULL}
      }, error = function(e){
        showNotification(paste0("Error (RL-05) : ",e), duration = 10, type = "error")
        NULL
      })
    })
    
    output$indexdfrl2 <- render_index_table({
      tryCatch({
        isolate(datos.aprendizaje <- updateData$datos.aprendizaje)
        isolate(variable.predecir <- updateData$variable.predecir)
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(tr("minimo",idioma),tr("q1",idioma),
                           tr("q3",idioma),tr("maximo",idioma))
        df2
      }
      , error = function(e){
        showNotification(paste0("Error (RL-06) : ",e), duration = 10, type = "error")
        NULL
      })
      
    })
    
  },ignoreInit = T)
  
}
    
## To be copied in the UI
# mod_linear_regression_ui("linear_regression_ui_1")
    
## To be copied in the server
# callModule(mod_linear_regression_server, "linear_regression_ui_1")
 
