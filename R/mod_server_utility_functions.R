# Update the different tables in the "shiny" application
update_table <- function(x = c("datos", "datos.aprendizaje", "datos.prueba"), output){
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
validate_data <- function(updateData, print = TRUE, idioma = "es") {
  if (is.null(updateData$variable.predecir) & print) {
    showNotification(translate("tieneVP",idioma), duration = 10, type = "error")
  }
  if (is.null(updateData$datos) & print) {
    showNotification(translate("tieneD",idioma), duration = 10, type = "error")
  }
  if (is.null(updateData$datos.aprendizaje) & print) {
    showNotification(translate("tieneDAP",idioma), duration = 10, type = "error")
  }
  return(!is.null(updateData$datos) & !is.null(updateData$variable.predecir) & !is.null(updateData$datos.aprendizaje))
}
 
