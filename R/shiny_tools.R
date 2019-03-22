
#' labelInput
#' 
#' @description label with identifier for language change 
#'
#' @details This function only work correctly on the server side because they need the css and js file.
#'
#' @param inputId The id of the label.
#' @param value Default Value of the label (optional). Default value is "".
#'
#' @return shiny.tag object
#' @keywords internal
#' 
labelInput <- function(inputId, value = ""){
  tags$span(`data-id` = inputId, value)
}

#' code.field
#' 
#' @description create an \code{\link{aceEditor}} wrapper with a button to execute the code
#' 
#' @details This function only work correctly on the server side because they need the css and js file.
#'
#' @param runid The id of the execute button.
#' @param fieldid The id of the code field.
#' @param ... Arguments to be passed to \code{\link{aceEditor}}
#'
#' @return shiny.tag object
#' @keywords internal
#' 
code.field <- function(runid, fieldid, ...) {
  tags$div(class = "box box-solid bg-black",
           tags$div(style = "text-align:right;padding-right: 10px;",
                    tags$button(id = runid, type = "button", class = "run-button action-button",
                                icon("play"), tags$a(labelInput("ejecutar"), style = "color:white"))),
           tags$div(class = "box-body",
                    aceEditor(fieldid, mode = "r", theme = "monokai", value = "", ...)))
}

#' infoBoxPROMiDAT
#' 
#' @description create a box for the information tab
#' 
#' @details This function only work correctly on the server side because they need the css and js file.
#'
#' @param title title of the box
#' @param value the content of the box
#' @param icon icon of the box
#'
#' @return shiny.tag object
#' @keywords internal
#' 
infoBoxPROMiDAT <- function(title, value, icon) {
  tags$div(class = "info-box bg-promidat",
           tags$span(class = "info-box-icon", icon),
           tags$div(class="info-box-content",
                    tags$span(class = "info-box-text", title),
                    tags$span(class = "info-box-number", value)))
}


#' inputRadio
#' 
#' @description Create a radio button input 
#'
#' @details This function only work correctly on the server side because they need the css and js file.
#'
#' @param inputId the id of the radio button
#' @param value a text value of the radio button
#' @param isSelected logical. If TRUE the radio button is selected
#'
#' @return shiny.tag object
#' @keywords internal
#' 
inputRadio <- function(inputId, value, isSelected) {
  res <- tags$input(type="radio", name=inputId, value=value)
  if(isSelected){
    res$attribs$checked <- "checked"
  }
  return(res)
}

#' radioButtonsTr
#' 
#' @description create multples with names for language change.
#' 
#' @details This function only work correctly on the server side because they need the css and js file.
#'
#' @param inputId the id of the group.
#' @param label label of the radio buttons group.
#' @param values vector with the values of each radio button.
#' @param names vector with the names of each radio button.
#'
#' @return shiny.tag object
#' @keywords internal
#' 
radioButtonsTr <- function(inputId, label, values, names){
  item <- function(i){
    tags$div(class="radio",tags$label(inputRadio(inputId, values[i], i == 1),tags$span(labelInput(names[i]))))
  }
  tags$div(id=inputId, class="form-group shiny-input-radiogroup shiny-input-container",
           tags$label(class="control-label", `for`= inputId, labelInput(label)),
           tags$div(class="shiny-options-group", lapply(1:length(values), item )))
}


#' tabsOptions
#'
#' @description create tabs options on panels.
#' 
#' @details This function only work correctly on the server side because they need the css and js file.
#'
#' @param buttons vector or list with icons of each tab.
#' @param widths vector or list with widths of each tab.
#' @param heights vector or list with heights of each tab.
#' @param tabs.content list with the content of each tab.
#'
#' @return shiny.tag
#' @keywords internal
#' 
tabsOptions <- function(buttons = list(icon("gear"), icon("terminal")), widths = c(50, 100),
                        heights = c(100, 50), tabs.content = list("", "")){
  res <- ""
  codeButtons <- ""
  cant <- length(buttons)
  if(cant == 1) {widgets <- c("center")}
  if(cant == 2) {widgets <- c("left", "right")}
  if(cant == 3) {widgets <- c("left", "center", "right")}
  if(cant == 4) {widgets <- c("left", "centerleft", "centeright", "right")}
  if(cant == 5) {widgets <- c("left", "centerleft", "center", "centeright", "right")}
  for(i in 1:cant){
    res <- paste0(res, tags$div(class = paste0("box-option box-option-", widgets[i]),
                                style = paste0("width:", widths[i], "%;height:", heights[i], "%;"),
                                tabs.content[[i]]), "\n")
    codeButtons <- paste0(codeButtons, "<button style='width:", 100/cant, "%' data-widget='",
                          widgets[i], "'>", buttons[[i]], "</button>\n")
  }
  res <- paste0(res, tags$div(class = "btn-options", style = "position:relative;",width = "100%", HTML(codeButtons)))
  return(tags$div(HTML(res)))
}

#' render.index.table
#' 
#' @description creates a reactive table for indices panels.
#'
#' @param table the data.frame to be converted
#'
#' @export
#' 
#' @examples
#' if(interactive()) {
#'    library(shiny)
#'    shinyApp(
#'       ui = fluidPage(fluidRow(column(12, tableOutput('tbl')))),
#'       server = function(input, output) {
#'          output$tbl = render.index.table(iris)
#'       }
#'    )
#' }
#'
render.index.table <- function(table){
  renderTable({table}, striped = TRUE, bordered = TRUE,  
              spacing = 'l', width = '100%',  digits = 5,
              align = 'c')
}



# Creates a table depending on the data entered
#' render.table.data
#'
#' @param data 
#' @param editable 
#' @param dom 
#' @param pageLength 
#' @param scrollY 
#' @param server 
#' @param language
#'
#' @return
#' @export
#'
#' @examples
#' if(interactive()) {
#'    library(shiny)
#'    library(DT)
#'    shinyApp(
#'       ui = fluidPage(fluidRow(column(12, DTOutput('tbl')))),
#'       server = function(input, output) {
#'          output$tbl = render.table.data(iris)
#'       }
#'    )
#' }
#'
renderizar.tabla.datos <- function(data, editable = TRUE, dom = "frtip", pageLength = 10, scrollY = "27vh", server = T, language = "es") {
  labelsNC <- ifelse(language == c("es", "es"), c("Numérico","Categórico"), c("Numerical","Categorical"))
  data <- head(data, n = 100)
  nombre.columnas <- c("ID", colnames(data))
  tipo.columnas <- sapply(colnames(data), function(i) ifelse(class(data[,i]) %in% c("numeric", "integer"),
                                                             paste0("<span data-id='numerico'>", labelsNC[1], "</span>"),
                                                             paste0("<span data-id='categorico'>", labelsNC[2], "</span>")))
  tipo.columnas <- lapply(tipo.columnas, function(i)tags$th(HTML(i)))
  sketch <- htmltools::withTags(table(tableHeader(nombre.columnas),
                                      tags$tfoot(tags$tr(tags$th(), tipo.columnas))))
  
  return(DT::renderDT(DT::datatable(data, selection = 'none', editable = editable,  container = sketch,
                                    options = list(dom = dom, pageLength = pageLength, scrollY = scrollY)), server = server))
}
