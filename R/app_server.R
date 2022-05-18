#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' 
app_server <- function( input, output, session ) {

  options(shiny.maxRequestSize = 209715200, width = 200, # 209715200 = 200 * 1024^2
          DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, 
                            language = list(search = HTML('<i class="fa fa-search"></i>'),
                                            emptyTable = "", zeroRecords = "",
                                            paginate = list("previous" = HTML('<i class="fa fa-backward"></i>'),
                                                            "next" = HTML('<i class="fa fa-forward"></i>'),
                                                            "first" =HTML('<i class="fa fa-fast-backward"></i>'),
                                                            "last" = HTML('<i class="fa fa-fast-forward"></i>')) )))
  
  
  # REACTIVE VALUES -------------------------------------------------------------------------------------------------------
  #updateData always has the same values of the global variables(datos, datos.prueba, datos.aprendizaje).
  updateData <- reactiveValues(originales   = NULL, datos = NULL, 
                               datos.prueba = NULL, datos.aprendizaje = NULL, 
                               variable.predecir = NULL, summary.var.pred = NULL, decimals = 2)
  
  codedioma <- reactiveValues(idioma = "es",
                              code   = list())
  
  modelos    <-  reactiveValues(rl  = NULL, rlr   = NULL, dt  = NULL, 
                                rf  = NULL, boost = NULL, knn = NULL, 
                                svm = NULL, rd    = NULL, nn  = NULL)
  
  new.data <- reactiveValues(originales.train = NULL, datos.train = NULL, variable.predecir = NULL,
                             nuevos = NULL, modelo = NULL, prediccion = NULL)
  
  
  
  # Enable/disable on load data
  observe({
    if(is.null(updateData$datos) || ncol(updateData$datos) < 1) {
      addClass(class = "disabled", selector = 'a[href^="#shiny-tab-parte1"]')
      shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
    }
    else{
      removeClass(class = "disabled", selector = 'a[href^="#shiny-tab-parte1"]')
      shinyjs::enable(selector = 'a[href^="#shiny-tab-parte1"]')
    }
    
    menu.selectors <- c('a[href^="#shiny-tab-parte2"]','a[href^="#shiny-tab-comparar"]',
                        'a[href^="#shiny-tab-poderPred"]')
    
    lapply(menu.selectors, function(i){
      if(is.null(updateData$datos.prueba) || ncol(updateData$datos.prueba) < 1) {
        addClass(class = "disabled", selector = i)
        shinyjs::disable(selector = i)
      } else {
        removeClass(class = "disabled", selector = i)
        shinyjs::enable(selector = i)
      }
    })
  })
  
  
  # CHANGE LANGUAGE -------------------------------------------------------------------------------------------------------
  
  #' Update on Language
  observeEvent(input$idioma, {
    codedioma$idioma = input$idioma
    etiquetas <- c(readeR::labels_readeR(), cambiar.labels())
    updateLabelInput(session, etiquetas, tr(etiquetas, input$idioma))
  })
  
  observeEvent(input$decimals_confg,{
    n <- input$decimals_confg
    if(is.numeric(n)){
      if(n >= 0 & n <= 20){
        updateData$decimals <- n
      }
      else{
        updateNumericInput(session,inputId = "decimals_confg",value = 2)
        updateData$decimals <- 2
      }
    }
    else{
      updateData$decimals <- 2
    }
  })
  
  
  # Update Code
  observeEvent(c(codedioma$code, input$idioma), {
    codigo <- codedioma$code
    lg     <- input$idioma
    
    keys <- c(
      'doccarga', 'doctt', 'doccv', 'docresumen', 'dochist', 'docqq', 
      'docnormal', 'docdisp', 'docdistnum', 'docdistcat', 'doccor',
      'docrename', 'doctrans', 'doceliminar', 'distpred', 'reglin', 'regpen', 'pares',
      'gcoeff', 'posibLanda', 'garbol', 'DT', 'rf', 'reglas')
    
    for (k in keys) {
      codigo <- gsub(k, tr(k, idioma = lg), codigo, fixed = T)
    }
    
    codigo.completo <- paste0(
      "library(XLConnect)\n", "library(caret)\n",
      "library(traineR)\n", "library(xgboost)\n",
      "library(echarts4r)\n", "library(readeR)\n\n"
    )
    for (cod in codigo) {
      codigo.completo <- paste0(codigo.completo, "\n", cod)
    }
    updateAceEditor(session, "fieldCode", value = codigo.completo)
  })
  
  output$btn_code <- downloadHandler(
    filename = "codigo.R",
    content = function(con) {
      write(input$fieldCode, con)
    }
  )
  
  # END THE SESSION -------------------------------------------------------------------------------------------------------
  
  # When the session closes
  onStop(function(){
    stopApp()
  })
  
  
  ###################################  Modules  ###############################
  #Carga de Datos
  readeR::mod_carga_datos_server("carga_datos_ui_1", updateData, modelos, codedioma, "regressoR")
  
  #Estadísticas Básicas
  readeR::mod_r_numerico_server("r_numerico_ui_1",         updateData, codedioma)
  readeR::mod_normal_server("normal_ui_1",                 updateData, codedioma)
  readeR::mod_dispersion_server("dispersion_ui_1",         updateData, codedioma)
  readeR::mod_distribuciones_server("distribuciones_ui_1", updateData, codedioma)
  readeR::mod_correlacion_server("correlacion_ui_1",       updateData, codedioma)
  callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1", updateData, codedioma)
  
  # Aprendizaje Supervisado
  callModule(mod_linear_regression_server,    "linear_regression_ui_1",    updateData, modelos, codedioma)
  callModule(mod_penalized_Regression_server, "penalized_Regression_ui_1", updateData, modelos, codedioma)
  callModule(mod_regression_trees_server,     "regression_trees_ui_1",     updateData, modelos, codedioma)
  #callModule(mod_random_forests_server,       "random_forests_ui_1",       updateData, modelos, codedioma)
  # callModule(mod_boosting_server,             "boosting_ui_1",             updateData, modelos, codedioma)
  # callModule(mod_KNN_server,                  "KNN_ui_1",                  updateData, modelos, codedioma)
  # callModule(mod_SVM_server,                  "SVM_ui_1",                  updateData, modelos, codedioma)
  # callModule(mod_dimension_reduction_server,  "dimension_reduction_ui_1",  updateData, modelos, codedioma)
  # callModule(mod_neural_networks_server,      "neural_networks_ui_1",      updateData, modelos, codedioma)
  # 
  # # Comparaación de Individuos
  # callModule(mod_model_comparison_server,     "model_comparison_ui_1",     updateData, modelos, codedioma)
  # 
  # # Predicción Ind. Nuevos
  # callModule(mod_new_data_predictions_server, "new_data_predictions_ui_1", updateData, new.data, codedioma)
  # 
  # # About
  # callModule(mod_information_page_server,     "information_page_ui_1", codedioma)
}
