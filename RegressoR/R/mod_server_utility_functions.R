# Update the different tables in the "shiny" application
update_table <- function(x = c("datos", "datos.aprendizaje", "datos.prueba")){
  if(any("datos" %in% x)){ # Change data table
    output$contents <- render_table_data(datos, editable = T, server=F)
  }
  if(any("datos.aprendizaje" %in% x)){ # Change learning data table
    output$contentsAprend <- render_table_data(datos.aprendizaje, editable=T, scrollY="15vh", server=F)
  }
  if(any("datos.prueba" %in% x)){ # Change test data table
    output$contentsPrueba <- render_table_data(datos.prueba, editable = T, scrollY="15vh", server=F)
  }
}

# Close a menu in the "shiny" application according to your tabName
close_menu <- function(tabname = NA, valor = T) {
  select <- paste0("a[href^='#shiny-tab-", tabname, "']")
  if(valor){
    shinyjs::hide(selector = "ul.menu-open")
    shinyjs::disable(selector = select)
  } else {
    shinyjs::enable(selector = select)
  }
}

# Common validation for all models
validate_data <- function(print = TRUE) {
  if (is.null(variable.predecir) & print) {
    showNotification(translate("tieneVP"), duration = 10, type = "error")
  }
  if (is.null(datos) & print) {
    showNotification(translate("tieneD"), duration = 10, type = "error")
  }
  if (is.null(datos.aprendizaje) & print) {
    showNotification(translate("tieneDAP"), duration = 10, type = "error")
  }
  return(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje))
}

# INITIAL SETTINGS ------------------------------------------------------------------------------------------------------

source("Utilities.R", local = TRUE, echo = FALSE )
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
    
## To be copied in the UI
# mod_server_utility_functions_ui("server_utility_functions_ui_1")
    
## To be copied in the server
# callModule(mod_server_utility_functions_server, "server_utility_functions_ui_1")
 
