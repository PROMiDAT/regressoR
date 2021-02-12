#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_report_ui <- function(id){
  
  header.report.panel <- column(width = 5, box(title = labelInput("reporte"), width = 12,
                                               textInput("textTitulo", value = "Sin Titulo", width = "100%", label = labelInput("titulo")),
                                               textInput("textNombre", value = "PROMiDAT", width = "100%", label = labelInput("nombre")),
                                               downloadButton("descargar", labelInput("descargar"), class = "center-button")))
  
  report.code.panel <- column(width = 7,box(title = labelInput("codreporte"), width = 12, height = "50vh",status = "primary", solidHeader = TRUE,
                                            collapsible = TRUE, aceEditor("fieldCodeReport", mode="markdown", value='', height = "43vh")))
  
  report.output.panel <- fluidRow(column(width = 12, box(title = labelInput("salida"), width = 12, height = "35vh", verbatimTextOutput("txtreport"))))
  
  
  page.generate.report <- tabItem(tabName = "reporte",
                                  fluidRow(header.report.panel ,
                                           report.code.panel),
                                  report.output.panel)
  
  ns <- NS(id)
  tagList(
 
  )
}
    
#' report Server Function
#'
#' @noRd 
mod_report_server <- function(input, output, session){
  ns <- session$ns
 
  
  # When the user enters the report page
  observeEvent(input$principal, {
    if(input$principal == "reporte"){
      extra_funs <- paste0(extract_code("pairs.panels"), "\n",
                           extract_code("dummy"),"\n",
                           extract_code("dummy.data.frame"),"\n",
                           extract_code("scatterplot3d"),"\n",
                           extract_code("printRandomForests"),"\n",
                           extract_code("printRandomForest"),"\n",
                           extract_code("printRandomForests"))
      updateAceEditor(session, "fieldCodeReport", value = word_report(input$textTitulo, input$textNombre, extra =  extra_funs))
    }
  })
  
  # When the user changes the report title
  observeEvent(input$textTitulo, {
    updateAceEditor(session, "fieldCodeReport", value = str_replace(input$fieldCodeReport, "title: '.*'", paste0("title: '", input$textTitulo, "'")))
  })
  
  # When the user changes the author of the report
  observeEvent(input$textNombre, {
    updateAceEditor(session, "fieldCodeReport", value = str_replace(input$fieldCodeReport, "author: '.*'", paste0("author: '", input$textNombre, "'")))
  })
  
  # When the user downloads the report
  output$descargar <- downloadHandler(
    filename = function() {
      paste(input$textTitulo,'-', input$textNombre, '.zip', sep='')
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL
      
      namermd <- paste(input$textTitulo,'-', input$textNombre, '.rmd', sep='')
      
      e <- options()$encoding
      options(encoding = regressoR:::enc)
      
      write.table(input$fieldCodeReport,namermd,row.names=F,col.names=F,quote=F)
      options(encoding = e)
      
      files <- c(namermd, files)
      
      src <- normalizePath(namermd)
      withCallingHandlers({
        overwrite_cat()
        salida.code <<- ""
        shinyjs::html("txtreport", salida.code)
        #merge_envir <- rlang::env_clone(get_env_report())
        #parent.env(merge_envir) <- options_regressor()$exe.envir
        out <- rmarkdown::render(src,  params = NULL, rmarkdown::word_document(highlight = "tango"), envir = get_env_report())
      },
      message = function(m) {
        salida.code <<- paste0(m$message, salida.code)
        shinyjs::html(id = "txtreport", html = salida.code)
      })
      
      recover_cat()
      file.rename(out, paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''))
      files <- c(paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''), files)
      
      zip::zip(file, files)
    }
  )
}
    
## To be copied in the UI
# mod_report_ui("report_ui_1")
    
## To be copied in the server
# callModule(mod_report_server, "report_ui_1")
 
