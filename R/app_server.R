#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  # The following variables belong to the server environment,
  # mean the following:
  # variable.predecir = The name of the variable to predict
  # datos = The full dataset
  # datos.prueba = The test dataset partition
  # datos.aprendizaje = The learning dataset partition
  # real.val = The values of the variable to predict (test data)
  
  # there are more variables in the "Global_Variables.R" file but these are the most important ones.
  
  # INITIAL SETTINGS ------------------------------------------------------------------------------------------------------
  #Loads all variables that the whole application needs
  load_global_variables()
  
  #Set the environment where the code will be executed. (.GlobalEnv recommended)
  options_regressor(exe.envir = .GlobalEnv)
  
  
  options(shiny.maxRequestSize = 209715200, width = 200, # 209715200 = 200 * 1024^2
          DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE, 
                            language = list(search = HTML('<i class="fa fa-search"></i>'),
                                            info = "", emptyTable = "", zeroRecords = "",
                                            paginate = list("previous" = HTML('<i class="fa fa-backward"></i>'),
                                                            "next" = HTML('<i class="fa fa-forward"></i>'),
                                                            "first" =HTML('<i class="fa fa-fast-backward"></i>'),
                                                            "last" = HTML('<i class="fa fa-fast-forward"></i>')) )))
  
  # The initial menu form
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte2"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-comparar"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-poderPred"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
  
  
  # REACTIVE VALUES -------------------------------------------------------------------------------------------------------
  #updateData always has the same values of the global variables(datos, datos.prueba, datos.aprendizaje).
  updateData <- reactiveValues(datos = NULL, datos.prueba = NULL, datos.aprendizaje = NULL, IndicesM = list(), idioma = "es")
  
  updatePlot <- reactiveValues(calc.normal = default_calc_normal(), 
                               normal      = NULL, 
                               disp        = NULL,
                               cor         = NULL, 
                               dya.num     = NULL, 
                               dya.cat     = NULL, 
                               poder.pred  = NULL,
                               poder.cat   = NULL,
                               poder.num   = NULL, 
                               poder.dens  = NULL, 
                               tablaCom    = FALSE)
  
  disp.ranges <- reactiveValues(x = NULL, y = NULL)
  
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
  callModule(mod_load_data_server,"load_data_ui_1",updateData)
  callModule(mod_r_numerico_server, "r_numerico_ui_1",updateData)
  callModule(mod_normal_server, "normal_ui_1",updateData, updatePlot,disp.ranges)
  callModule(mod_dispersion_server, "dispersion_ui_1", updateData, updatePlot,disp.ranges)
  callModule(mod_distribuciones_server, "distribuciones_ui_1", updateData, updatePlot,disp.ranges)
  callModule(mod_correlacion_server, "correlacion_ui_1", updateData, updatePlot,disp.ranges)
  callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1", updateData, updatePlot,disp.ranges)
  callModule(mod_linear_regression_server, "linear_regression_ui_1",updateData, updatePlot)
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
