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
  
  # there are more variables in the "global" file but these are the most important ones.
  
  # INITIAL SETTINGS ------------------------------------------------------------------------------------------------------
  options_regressor(exe.envir = environment())
  
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
  
  # Some code fields that are not parameter-dependent
  updateAceEditor(session, "fieldCodeResum", value = code_summary())
  updateAceEditor(session, "fieldModelCor" , value = cor_model())
  updateAceEditor(session, "fieldFuncNum"  , extract_code("numerical_distribution"))
  updateAceEditor(session, "fieldFuncCat"  , extract_code("categorical_distribution"))
  
  # REACTIVE VALUES -------------------------------------------------------------------------------------------------------
  
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
  callModule(mod_load_data_server,"load_data_ui_1")
}
