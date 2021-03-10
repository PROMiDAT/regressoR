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
  
  #Set the environment where the code will be executed. (.GlobalEnv recommended)
  options_regressor(exe.envir = .GlobalEnv)
  
  
  clean_report()
  
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
  updateData <- reactiveValues(datos = NULL, datos.prueba = NULL, datos.aprendizaje = NULL)
  
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
  observeEvent(c(input$idioma), {
    options_regressor(language = input$idioma)
    updateLabelInput(session, cambiar.labels(), tr(cambiar.labels(), input$idioma))
  })
    
    
  ###################################  Modules  ###############################
  callModule(mod_load_data_server,"load_data_ui_1",updateData)
  callModule(mod_r_numerico_server, "r_numerico_ui_1",updateData)
  callModule(mod_normal_server, "normal_ui_1",updateData, updatePlot,disp.ranges)
  callModule(mod_dispersion_server, "dispersion_ui_1", updateData, updatePlot,disp.ranges)
  callModule(mod_distribuciones_server, "distribuciones_ui_1", updateData, updatePlot,disp.ranges)
  callModule(mod_correlacion_server, "correlacion_ui_1", updateData, updatePlot,disp.ranges)
  callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1", updateData, updatePlot,disp.ranges)
}
