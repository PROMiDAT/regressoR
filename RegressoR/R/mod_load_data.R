#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_data_ui <- function(id){
  ns <- NS(id)
  
  data.upload.panel <- tabPanel(title = labelInput("cargar"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                checkboxInput(ns('header'), labelInput("header"), TRUE),
                                checkboxInput(ns('rowname'), labelInput("Rownames"), TRUE),
                                radioButtons(ns('sep'), labelInput("separador"), inline = T,
                                             choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')),
                                radioButtons(ns('dec'), labelInput("separadordec"), c(',', '.'), inline = T),
                                radioSwitch(ns("deleteNA"), "eliminana", c("eliminar", "imputar")),
                                fileInput(ns('file1'), label =  labelInput("cargarchivo"), placeholder = "", buttonLabel =  labelInput("subir"), width = "100%",
                                          accept = c('text/csv', '.csv')),
                                actionButton(ns("loadButton"), labelInput("cargar"), width = "100%"),
                                br(),br(),
                                aceEditor(ns("fieldCodeData"), mode = "r", theme = "monokai", value = "", height = "13vh", readOnly = T))
  
  tansform.data.panel <- tabPanel(title = labelInput("trans"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                  DT::dataTableOutput(ns('transData')),
                                  br(),br(),
                                  actionButton(ns("transButton"), labelInput("aplicar"), width = "100%"),
                                  br(),br(),
                                  aceEditor(ns("fieldCodeTrans"), mode = "r", theme = "monokai", value = "", height = "10vh",  readOnly = T))
  
  data.segment.panel <- tabPanel(title = labelInput("configuraciones"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                 fluidRow(column(id = ns("colSemilla"),width = 6, numericInput(ns("semilla"), labelInput("semilla"), "NULL", width = "100%")), br(),
                                          column(width = 6, switchInput(inputId = ns("permitir.semilla"), onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                                        label = "", onLabel = labelInput("habilitada"), offLabel = labelInput("deshabilitada"), labelWidth = "100%",
                                                                        inline = T,size = "large"))),
                                 selectInput(inputId = ns("sel.predic.var"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"),
                                 sliderInput(ns("segmentacionDatosA"), labelInput("propA"),width = "100%",
                                             min = 5, max = 95, value = 70, step = 5),
                                 sliderInput(ns("segmentacionDatosT"), labelInput("propP"), width = "100%",
                                             min = 5, max = 95, value = 30, step = 5),
                                 actionButton(ns("segmentButton"), labelInput("generar"), width = "100%"),
                                 br(),br(),
                                 aceEditor(ns("fieldCodeSegment"), mode = "r", theme = "monokai", value = "", height = "8vh",  readOnly = T))
  
  show.data <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
                   DT::DTOutput(ns('contents')), hr(),
                   downloadButton(ns("downloaDatos"), labelInput("descargar"), width = "100%"))
  
  show.learning.data <- box(title = labelInput("dataA"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
                            DT::DTOutput(ns('contentsAprend')), hr(),
                            downloadButton(ns("downloaDatosA"), labelInput("descargar"), width = "100%"))
  
  show.test.data <- box(title = labelInput("dataP"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
                        DT::DTOutput(ns('contentsPrueba')), hr(),
                        downloadButton(ns("downloaDatosP"), labelInput("descargar"), width = "100%"))
  
  page.load.data <- tabItem(tabName = "cargar",
                            fluidRow(column(width = 5, tabBox(id =ns("tabs"), title = NULL, width = 12,
                                                              data.upload.panel,
                                                              tansform.data.panel,
                                                              data.segment.panel)),
                                     column(width = 7, show.data)),
                            conditionalPanel(condition = paste0("input.tabs == '", labelInput("configuraciones"),"'"),
                                             fluidRow(column(width = 6, show.learning.data),
                                                      column(width = 6, show.test.data)), ns = ns) )
  
  tagList(
    page.load.data
  )
}
    
#' load_data Server Function
#'
#' @noRd 
mod_load_data_server <- function(input, output, session,updateData){
  ns <- session$ns
  
  # Executes the data upload code
  upload_data <- function(codigo.carga = "") {
    tryCatch({
      isolate(exe(codigo.carga))
      if(ncol(datos) <= 1) {
        showNotification(translate("errorCData"), duration = 10, type = "error")
        return(NULL)
      }
      #new_report(datos.originales, input$file1$name)
    },
    error = function(e) {
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    })
  }
  
  # Executes data cleanup code (cleaning of raw data)
  clean_data <- function(){
    if (any(is.na(datos))) {
      tryCatch({
        codigo.na <- paste0(code_NA(deleteNA = input$deleteNA), "\n", "datos <- datos.originales")
        isolate(exe(codigo.na))
        
        #insert_report("na.delete", "Imputaci\u00F3n de Datos", codigo.na,"\nhead(datos)\nstr(datos)")
        
      }, error = function(e) {
        showNotification(paste0("Error (NA): ", e), duration = 10, type = "error")
        datos <<- NULL
        datos.originales <<- NULL
        return(NULL)
      })
    } else {
      codigo.na <- ""
    }
    return(codigo.na)
  }
  
  #Executes each time 'datos' changes
  observeEvent(updateData$datos,{
    updateSelectInput(session, "sel.predic.var", choices = rev(colnames_empty(var_numerical(updateData$datos))))
  })
  
  # Use and show the codes to transform the data
  transformar.datos <- function() {
    var.noactivas <- c()
    code.res <- "datos <- datos.originales \n"
    for (col_name in colnames(datos.originales)) {
      if (input[[paste0("box", col_name, contador)]]) {
        if (input[[paste0("sel", col_name, contador)]] == "categorico" & class(datos.originales[, col_name]) %in% c("numeric", "integer")) {
          code.res <- paste0(code.res, code_transf(col_name, "categorico"), "\n")
        }
        if (input[[paste0("sel", col_name, contador)]] == "numerico" & !(class(datos.originales[, col_name]) %in% c("numeric", "integer"))) {
          code.res <- paste0(code.res, code_transf(col_name, "numerico"), "\n")
        }
        if (input[[paste0("sel", col_name, contador)]] == "disyuntivo") {
          code.res <- paste0(code.res, code_transf(col_name, "disyuntivo"), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, col_name)
      }
    }
    isolate(exe(code.res))
    code.res <- paste0(code.res, "\n")
    if (length(var.noactivas) > 0) {
      isolate(exe(code_deactivate(var.noactivas)))
      code.res <- paste0(code.res, code_deactivate(var.noactivas))
    }
    
    #new_section_report()
    #insert_report("transformar.datos","Transformando Datos", code.res,"\nstr(datos)")
    return(code.res)
  }
  
  
  # Clears model data
  delete_models <- function(flag.datos = TRUE) {
    if (flag.datos) {
      datos.prueba <<- NULL
      datos.aprendizaje <<- NULL
      variable.predecir <<- NULL
      real.val <<- NULL
    }
    
    updateData$IndicesM <- list()
    
    #rm(list = nombres.modelos, envir = options_regressor()$exe.envir)
    #nombres.modelos <<- c()
    
    updateCheckboxGroupButtons(session, inputId = "select.models",
                               choices = c(" ---- " = "NoDisponible"),
                               size = "sm", status = "primary",
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                no = icon("remove", lib = "glyphicon")))
    
    updateSelectInput(session,"kernel.knn", selected = "optimal")
  }
  
  # When the load data button is pressed
  observeEvent(input$loadButton, {
    codigo.carga <- code_load(row.names = input$rowname, 
                              path = input$file1$datapath,
                              sep = input$sep, 
                              sep.dec = input$dec, 
                              header = input$header)
    
    upload_data(codigo.carga)
    
    codigo.na <- clean_data()
    
    updateAceEditor(session, "fieldCodeData", value = paste0(codigo.carga, "\n", codigo.na))
    
    updateData$datos <- datos
    
    delete_models()
    
    close_menu("parte1"   , is.null(datos))
    close_menu("parte2"   , is.null(datos.aprendizaje))
    close_menu("comparar" , is.null(datos.aprendizaje))
    close_menu("poderPred", is.null(datos.aprendizaje))
    
    update_table(output = output)
  }, priority = 4)
  
  # When the button to transform data is pressed
  observeEvent(input$transButton, {
    code.res <- transformar.datos()
    
    updateAceEditor(session, "fieldCodeTrans", value = code.res)
    
    updateData$datos <- datos
    
    delete_models()
    
    close_menu("parte1"   , is.null(datos))
    close_menu("parte2"   , is.null(datos.aprendizaje))
    close_menu("comparar" , is.null(datos.aprendizaje))
    close_menu("poderPred", is.null(datos.aprendizaje))
    
    update_table(output = output)
  }, priority = 4)
  
  # Show the select box of the panel to transform data
  update_trans <- eventReactive(input$loadButton, {
    contador <<- contador + 1
    if(!is.null(datos) && ncol(datos) > 0){
      res <- data.frame(Variables = colnames(datos), Tipo = c(1:ncol(datos)), Activa = c(1:ncol(datos)))
      res$Tipo <- sapply(colnames(datos), function(i)
        paste0('<select id=', ns(paste0("sel", i, contador)), '> <option value="categorico">',translate("categorico"),'</option>',
               '<option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric", "integer"),
                                                   ' selected="selected"', ""),'>', translate("numerico"),
               '</option> <option value="disyuntivo">',translate("disyuntivo"),'</option> </select>'))
      res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id=', ns(paste0("box", i, contador)), ' checked/>'))
    } else {
      res <- as.data.frame(NULL)
      showNotification(translate("tieneCData"), duration = 10, type = "error")
    }
    return(res)
  })
  
  # Change the table with the options of the transform panel
  output$transData <- DT::renderDT({sketch <- htmltools::withTags(table(tags$thead(tags$tr(tags$th(tags$span(`data-id` = "variables", "Variables")),
                                                                                           tags$th(tags$span(`data-id` = "tipo", "Tipo")),
                                                                                           tags$th(tags$span(`data-id` = "activa", "Activa"))))))
  DT::datatable(update_trans(),
                escape = FALSE, selection = "none", container = sketch,
                options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = "45vh"), rownames = F,
                callback = JS("table.rows().every(function(i, tab, row) {
                                                        var $this = $(this.node());
                                                        $this.attr('id', this.data()[0]);
                                                        $this.addClass('shiny-input-checkbox');});
                                                        Shiny.unbindAll(table.table().node());
                                                        Shiny.bindAll(table.table().node());"))
  }, server = FALSE)
  
  # The download of full data
  output$downloaDatos <- downloadHandler(
    filename = function() {
      input$file1$name
    },
    content = function(file) {
      write.csv(datos, file, row.names = input$rowname)
    }
  )
  
  
  # SPLIT DATA PAGE -------------------------------------------------------------------------------------------------------
  
  # Executes data segmentation code
  segmentar.datos <- function(codigo) {
    tryCatch({
      isolate(exe(codigo))
      updateAceEditor(session, "fieldCodeSegment", value = codigo)
    }, error = function(e) {
      showNotification(paste0(translate("errorSeg"), e), duration = 15, type = "error")
    })
  }
  
  # When the segment data button is pressed
  observeEvent(input$segmentButton, {
    if(input$sel.predic.var != ""){
      codigo <- partition_code("datos", input$segmentacionDatosA,
                               input$sel.predic.var,
                               input$semilla,
                               input$permitir.semilla)
      
      semilla       <<- input$permitir.semilla
      knn.stop.excu <<- FALSE
      rf.stop.excu  <<- FALSE
      
      
      segmentar.datos(codigo)
      updateData$datos.aprendizaje <- datos.aprendizaje
      updateData$datos.prueba <- datos.prueba

      #new_section_report()
      # insert_report("segmentar.datos","Datos de Aprendizaje",codigo, "\nhead(datos.aprendizaje)", interpretation = FALSE)
      # insert_report("segmentar.datos","Datos de Prueba","head(datos.prueba)", add = TRUE, interpretation = FALSE)
      
      delete_models(FALSE)
      
    } else {
      showNotification(translate("tieneSVP"), duration = 15, type = "error")
    }
    
    close_menu("parte2",    is.null(datos.aprendizaje))
    close_menu("comparar",  is.null(datos.aprendizaje))
    close_menu("poderPred", is.null(datos.aprendizaje))
    
    update_table(c("datos.aprendizaje", "datos.prueba"),output = output)
    
  },priority = 5)
  
  # When user press enable or disable the seed
  observeEvent(input$permitir.semilla, {
    if (input$permitir.semilla) {
      shinyjs::enable("semilla")
    } else {
      shinyjs::disable("semilla")
    }
  })
  
  # When the data provider bar changes (Learning Data)
  observeEvent(input$segmentacionDatosA, {
    updateSliderInput(session, "segmentacionDatosT", value = 100 - input$segmentacionDatosA)
  })
  
  # When the data provider bar changes (Test Data)
  observeEvent(input$segmentacionDatosT, {
    updateSliderInput(session, "segmentacionDatosA", value = 100 - input$segmentacionDatosT)
  })
  
  # Download the learning table
  output$downloaDatosA <- downloadHandler(
    filename = function(){
      paste0("(",translate("dataA"),")",input$file1$name)
    },
    content = function(file) {
      write.csv(datos.aprendizaje, file, row.names = input$rowname)
    }
  )
  
  # Download the test table
  output$downloaDatosP <- downloadHandler(
    filename = function() {
      paste0("(",translate("dataP"),")",input$file1$name)
    },
    content = function(file) {
      write.csv(datos.prueba, file, row.names = input$rowname)
    }
  )
 
}
    
## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")
    
## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
 
