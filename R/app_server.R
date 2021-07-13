#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  load_global_variables()
  
  options(shiny.maxRequestSize = 209715200, width = 200, # 209715200 = 200 * 1024^2
          DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE, 
                            language = list(search = HTML('<i class="fa fa-search"></i>'),
                                            info = "", emptyTable = "", zeroRecords = "",
                                            paginate = list("previous" = HTML('<i class="fa fa-backward"></i>'),
                                                            "next" = HTML('<i class="fa fa-forward"></i>'),
                                                            "first" =HTML('<i class="fa fa-fast-backward"></i>'),
                                                            "last" = HTML('<i class="fa fa-fast-forward"></i>')) )))
  
  
  # REACTIVE VALUES -------------------------------------------------------------------------------------------------------
  #updateData always has the same values of the global variables(datos, datos.prueba, datos.aprendizaje).
  updateData <- reactiveValues(originales = NULL, datos = NULL, 
                               datos.prueba = NULL, datos.aprendizaje = NULL, 
                               variable.predecir = NULL,
                               IndicesM = list(), idioma = "es")
  
  modelos    <-  reactiveValues(mdls = list(rl = NULL, rlr= NULL,
                                            dt = NULL, rf = NULL,
                                            boosting = NULL, xgb = NULL,
                                            knn      = NULL, svm = NULL, nn = NULL),
                                metrics = list())
  
  updatePlot <- reactiveValues(dya.num     = NULL, 
                               dya.cat     = NULL, 
                               poder.pred  = NULL,
                               poder.cat   = NULL,
                               poder.num   = NULL, 
                               poder.dens  = NULL, 
                               tablaCom    = FALSE)
  
  
  
  #' Enable/disable on load data
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
  
  # When the user changes the language
  observeEvent(input$idioma, {
    if(updateData$idioma != input$idioma){
      updateData$idioma <- input$idioma
    }
    options_regressor(language = input$idioma)
    updateLabelInput(session, cambiar.labels(), tr(cambiar.labels(), input$idioma))
  })
  
  
  
  # END THE SESSION -------------------------------------------------------------------------------------------------------
  
  # When the session closes
  onStop(function(){
    options_regressor(exe.envir = NULL)
    
    #Eliminamos todas las variables y funciones que se crearon
    rm(list = ls(.GlobalEnv,all.names = TRUE),envir = .GlobalEnv)
    
    stopApp()
  })
    
    
  ###################################  Modules  ###############################
  callModule(mod_carga_datos_server,"carga_datos_ui_1",updateData, modelos)
  callModule(mod_r_numerico_server, "r_numerico_ui_1",updateData)
  callModule(mod_normal_server, "normal_ui_1",updateData)
  callModule(mod_dispersion_server, "dispersion_ui_1", updateData)
  callModule(mod_distribuciones_server, "distribuciones_ui_1", updateData)
  callModule(mod_correlacion_server, "correlacion_ui_1", updateData)
  callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1", updateData)
  callModule(mod_linear_regression_server, "linear_regression_ui_1",updateData, modelos)
  callModule(mod_penalized_Regression_server, "penalized_Regression_ui_1",updateData, updatePlot)
  callModule(mod_regression_trees_server, "regression_trees_ui_1",updateData, updatePlot)
  callModule(mod_random_forests_server, "random_forests_ui_1",updateData, updatePlot)
  callModule(mod_boosting_server, "boosting_ui_1",updateData, updatePlot)
  callModule(mod_KNN_server, "KNN_ui_1",updateData, updatePlot)
  callModule(mod_SVM_server, "SVM_ui_1",updateData, updatePlot)
  callModule(mod_dimension_reduction_server, "dimension_reduction_ui_1",updateData, updatePlot)
  callModule(mod_neural_networks_server, "neural_networks_ui_1",updateData, updatePlot)
  callModule(mod_model_comparison_server, "model_comparison_ui_1",updateData,updatePlot)
  callModule(mod_new_data_predictions_server, "new_data_predictions_ui_1",updateData,updatePlot)
  callModule(mod_information_page_server, "information_page_ui_1")
}