#' new_data_predictions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_new_data_predictions_ui <- function(id){
  
  ns <- NS(id)
  
  btn_style <- "width: 100%;background-color: #3d8dbc;color: white;"
  btn_style_hidden <- "width: 100%;background-color: #3d8dbc;color: white; display:none;"
  
  
  # Data display
  
  show.data1 <- box(title = labelInput("data"), status = "primary", width = 12,
                    solidHeader = TRUE, collapsible = TRUE,
                    withLoader(DT::dataTableOutput(ns('tabladatos1')),type = "html", loader = "loader4"))
  
  show.data2 <- box(title = labelInput("data"), status = "primary", width = 12,
                    solidHeader = TRUE, collapsible = TRUE,
                    withLoader(DT::dataTableOutput(ns('tabladatos2')),type = "html", loader = "loader4"))
  
  show.data3 <- box(title = labelInput("data"), status = "primary", width = 12,
                    solidHeader = TRUE, collapsible = TRUE,
                    withLoader(DT::dataTableOutput(ns('tabladatos3')),type = "html", loader = "loader4"))
  
  # Loading and transforming data
  
  data.upload.panel.pred <- div(id = ns("seccion1"),
                                fluidRow(
                                  column(width = 11,
                                         tabBox(width = 12,
                                                tabPanel(title =labelInput("cargarDatos"), 
                                                         fluidRow(
                                                           column(width = 5,
                                                                  checkboxInput(ns('headerNPred'), labelInput("header"), TRUE),
                                                                  checkboxInput(ns('rownameNPred'), labelInput("Rownames"), TRUE),
                                                                  radioButtons(ns('sepNPred'), labelInput("separador"), inline = T, 
                                                                               choiceValues = c(';', ',', '\t'), choiceNames = c(';', ',', 'TAB')),
                                                                  radioButtons(ns('decNPred'),labelInput("separadordec"), inline = T,
                                                                               choiceValues = c(',', '.'), choiceNames = c(',', '.')),
                                                                  radioSwitch(ns("deleteNAnPred"), "eliminana", c("eliminar", "imputar")),
                                                                  fileInput(ns('file2'), label = labelInput("cargarchivo"), placeholder = "", 
                                                                            buttonLabel =  labelInput("subir"), width = "100%",
                                                                            accept = c('text/csv', '.csv')),
                                                                  actionButton(ns("loadButtonNPred"), labelInput("cargar"), width = "100%")),
                                                           column(width = 7, show.data1))))),
                                  column(width = 1, actionButton(inputId = ns("btn_next1"),
                                                                 label = NULL, icon = icon("forward"), style = btn_style_hidden) )))
  
  
  transform.data.panel <-   div(id = ns("seccion2"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev1"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 10,tabBox(
      width = 12,
      tabPanel(title = labelInput("transDatos"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
               fluidRow(column(width = 5,
                               uiOutput(ns('transData1')), hr(),
                               actionButton(ns('transButton1'), labelInput("aplicar"), width = "100%")),
                        column(width = 7, show.data2))))),
    
    column(width = 1, actionButton(inputId = ns("btn_next2"),
                                   label = NULL, icon = icon("forward"), style = btn_style) )))
  
  
  
  data.upload.panel.pred2 <- div(id = ns("seccion4"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev3"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 10,tabBox(
      width = 12,
      tabPanel(title = labelInput("cargarNuev"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
               fluidRow(column(width = 5,
                               checkboxInput(ns('headerNPred2'), labelInput("header"), TRUE),
                               checkboxInput(ns('rownameNPred2'),  labelInput("Rownames"), TRUE),
                               radioButtons(ns('sep.nPred2'), labelInput("separador"), inline = T, choiceValues = c(';', ',', '\t'), choiceNames = c(';', ',', 'TAB')),
                               radioButtons(ns('dec.nPred2'),labelInput("separadordec"), inline = T,choiceValues = c(',', '.'), choiceNames = c(',', '.')),
                               radioSwitch(ns("deleteNAnPred2"), "eliminana", c("eliminar", "imputar")),
                               fileInput(ns('file3'), label = labelInput("cargarchivo"), placeholder = "", buttonLabel = labelInput("subir"), width = "100%",
                                         accept = c('text/csv', '.csv')),
                               actionButton(ns("loadButtonNPred2"), labelInput("cargar"), width = "100%")),
                        column(width = 7, show.data3))))),
    column(width = 1, actionButton(inputId = ns("btn_next4"),
                                   label = NULL, icon = icon("forward"), style = btn_style_hidden) )))
  
  # Model Options
  
  options.rl.pred <- list() # Vacio
  
  options.rlr.pred <- fluidRow(column(selectInput(inputId = ns("alpha.rlr"), label = labelInput("selectAlg"),selected = 1,
                                                  choices = list("Ridge" = 0, "Lasso" = 1)),width = 3),
                               column(width = 3,radioSwitch(id = ns("switch_scale_rlr"), label = "escal", 
                                                            names = c("si", "no"))),
                               column(id = ns("colManualLanda"),width = 3, numericInput(ns("log_landa"), labelInput("log_landa"),value = 2, min = 0, "NULL", width = "100%")),
                               column(width = 3, radioSwitch(id = ns("permitir_landa"), label = "",
                                                             names = c("manual", "automatico"), val.def = FALSE)))
  
  options.rd.pred <-  fluidRow(column(selectInput(inputId = ns("mode.rd.pred"), label = labelInput("selectAlg"),selected = 0,
                                                  choices = list("ACP" = 0, "MCP" = 1)),width = 3),
                               column(br(), switchInput(inputId = ns("switch.scale.rd.pred"), onStatus = "success", offStatus = "danger", value = T,
                                                        label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=3),
                               column(id = ns("colManualCom"),width = 3, numericInput(ns("ncomp.rd.pred"), labelInput("ncomp"),value = 2, min = 0, "NULL", width = "100%")), br(),
                               column(width = 3, switchInput(inputId = ns("permitir.ncomp.pred"), onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                             label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%")))
  
  options.knn.pred <- fluidRow(column(width = 3, br() , switchInput(inputId = ns("switch_scale_knn"), onStatus = "success", offStatus = "danger", value = T,
                                                                    label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%", width = "100%")),
                               column(width = 3, numericInput(ns("kmax_knn"), labelInput("kmax"), min = 1,step = 1, value = 7,width="100%")),
                               column(width = 3, selectInput(inputId = ns("kernel_knn"), label = labelInput("selkernel") ,selected = 1, width="100%",
                                                             choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                                          "triweight", "cos","inv","gaussian"))),
                               column(width = 3,numericInput(ns("distance_knn"), labelInput("distknn"), min = 1,step = 1, value = 2) ))
  
  options.svm.pred <- fluidRow(column(width = 6, br(), switchInput(inputId = ns("switch.scale.svm.pred"), onStatus = "success", offStatus = "danger", value = T,
                                                                   label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%", width = "100%")),
                               column(width = 6, selectInput(inputId = ns("kernel.svm.pred"), label = labelInput("selkernel"), selected = "radial", width="100%",
                                                             choices =  c("linear", "polynomial", "radial", "sigmoid"))))
  
  options.dt.pred <- fluidRow(column(width = 6, numericInput(ns("minsplit.dt.pred"), labelInput("minsplit"), 20, width = "100%",min = 1)),
                              column(width = 6, numericInput(ns("maxdepth.dt.pred"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1)))
  
  options.rf.pred <- fluidRow(column(width = 6, numericInput(ns("ntree.rf.pred"), labelInput("numTree"), 20, width = "100%", min = 0)),
                              column(width = 6, numericInput(ns("mtry.rf.pred"),labelInput("numVars"),1, width = "100%", min = 1)))
  
  options.boosting.pred <- list(fluidRow(column(width = 4, numericInput(ns("iter.boosting.pred"), labelInput("numTree"), 300, width = "100%",min = 1)),
                                         column(width = 4, numericInput(ns("shrinkage.boosting.pred"),labelInput("shrinkage"), 0.1, width = "100%",min = 0.0001)),
                                         column(width = 4, selectInput(inputId = ns("tipo.boosting.pred"), label = labelInput("selectAlg"),selected = 1, width = "100%",
                                                                       choices =  c("gaussian", "laplace", "tdist")))))
  
  options.nn.pred <-list(fluidRow(column(numericInput(ns("threshold.nn.pred"),labelInput("threshold"),
                                                      min = 0, step = 0.01, value = 0.1), width = 4),
                                  column(numericInput(ns("stepmax.nn.pred"),labelInput("stepmax"),
                                                      min = 100, step = 100, value = 5000), width = 4),
                                  column(sliderInput(inputId = ns("cant.capas.nn.pred"), min = 1, max = 10,
                                                     label = labelInput("selectCapas"), value = 5), width = 4)),
                         fluidRow(lapply(1:10, function(i) tags$span(numericInput(ns(paste0("nn.cap.pred.",i)), NULL,
                                                                                  min = 1, step = 1, value = 2),
                                                                     class = "mini-numeric-select"))))
  
  options.model <- list(selectInput(inputId = ns("sel.predic.var.nuevos"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"),
                        radioGroupButtons(ns("selectModelsPred"), labelInput("selectMod"), 
                                          list("<span data-id=\"rl\"></span>" = "rl",
                                               "<span data-id=\"rlr\"></span>" = "rlr",
                                               "<span data-id=\"rd\"></span>" = "rd",
                                               "<span data-id=\"knn\"></span>" = "knn",
                                               "<span data-id=\"dt\"></span>" = "dt",
                                               "<span data-id=\"rf\"></span>" = "rf",
                                               "<span data-id=\"boost\"></span>" = "boosting",
                                               "<span data-id=\"svm\"></span>" = "svm",
                                               "<span data-id=\"rd\"></span>" = "rd",
                                               "<span data-id=\"nn\"></span>" = "nn"),
                                          size = "sm", status = "primary",individual = FALSE, justified = FALSE, selected = "knn",
                                          checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                           no = icon("remove", lib = "glyphicon"))))
  
  tabs.models  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                              tabs.content = list(list(codigo.monokai(ns("fieldPredNuevos"), height = "7vh"))))
  
  
  create.pred.model.panel <- div(id = ns("seccion3"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev2"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 10,tabBox(
      width = 12,
      tabPanel(title = labelInput("seleParModel"),solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE, value = "crearModelo",
               options.model,
               conditionalPanel(condition =  "input.selectModelsPred == 'rl'",
                                options.rl.pred, ns = ns),
               conditionalPanel(condition =  "input.selectModelsPred == 'rlr'",
                                options.rlr.pred, ns = ns),
               conditionalPanel(condition =  "input.selectModelsPred == 'knn'",
                                options.knn.pred, ns = ns),
               conditionalPanel(condition =  "input.selectModelsPred == 'dt'",
                                options.dt.pred, ns = ns),
               conditionalPanel(condition =  "input.selectModelsPred == 'rf'",
                                options.rf.pred, ns = ns),
               conditionalPanel(condition =  "input.selectModelsPred == 'boosting'",
                                options.boosting.pred, ns = ns),
               conditionalPanel(condition =  "input.selectModelsPred == 'svm'",
                                options.svm.pred, ns = ns),
               conditionalPanel(condition =  "input.selectModelsPred == 'nn'",
                                options.nn.pred, ns = ns),
               conditionalPanel(condition =  "input.selectModelsPred == 'rd'",
                                options.rd.pred, ns = ns),
               withLoader(verbatimTextOutput(ns("txtPredNuevos")), type = "html", loader = "loader4"),
               actionButton(ns("PredNuevosBttnModelo"), labelInput("generarM"), 
                            width  = "100%", style = "background-color:#CBB051;color:#fff;margin-top:9px;")),
      tabs.models)),
    column(width = 1, actionButton(inputId = ns("btn_next3"),
                                   label = NULL, icon = icon("forward"), style = btn_style_hidden) )
  ))
  
  
  
  
  tabs.models2  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                               tabs.content = list(codigo.monokai(ns("fieldCodePredPN"), height = "7vh")))
  
  prediccion.pred.panel <- div(id = ns("seccion5"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev4"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 11,tabBox(
      width = 12,
      tabPanel(title = labelInput("predicnuevos"), value = "predicModelo",
               DT::dataTableOutput(ns("PrediTablePN")),
               hr(),
               downloadButton(ns("downloaDatosPred"), labelInput("descargar"), style = "width:100%"),
               actionButton(ns("predecirPromidat"), "preditc",style="display:none;")),
      tabs.models2))))
  
  
  
  tagList(
    data.upload.panel.pred,
    transform.data.panel,
    create.pred.model.panel,
    data.upload.panel.pred2,
    prediccion.pred.panel
  )
}

#' new_data_predictions Server Function
#'
#' @noRd 
mod_new_data_predictions_server <- function(input, output, session, updateData, new.data, modelos){
  ns <- session$ns
  
  observeEvent(input$btn_next1,{
    shinyjs::hide("seccion1",anim = T)
    shinyjs::show("seccion2",anim = T)
  })
  
  observeEvent(input$btn_prev1,{
    shinyjs::show("seccion1",anim = T)
    shinyjs::hide("seccion2",anim = T)
  })
  
  observeEvent(input$btn_next2,{
    shinyjs::hide("seccion2",anim = T)
    shinyjs::show("seccion3",anim = T)
  })
  
  observeEvent(input$btn_prev2,{
    shinyjs::show("seccion2",anim = T)
    shinyjs::hide("seccion3",anim = T)
  })
  
  observeEvent(input$btn_next3,{
    shinyjs::hide("seccion3",anim = T)
    shinyjs::show("seccion4",anim = T)
  })
  
  observeEvent(input$btn_prev3,{
    shinyjs::show("seccion3",anim = T)
    shinyjs::hide("seccion4",anim = T)
  })
  
  observeEvent(input$btn_next4,{
    shinyjs::hide("seccion4",anim = T)
    shinyjs::show("seccion5",anim = T)
    #Realozar prediccion
    shinyjs::click("predecirPromidat")
  })
  
  observeEvent(input$btn_prev4,{
    shinyjs::show("seccion4",anim = T)
    shinyjs::hide("seccion5",anim = T)
  })
  
  
  default.values.inputs <- function(){
    
    #----------------rlr----------------
    updateSelectInput(session, "alpha.rlr",selected = 1)
    updateRadioSwitch(session,"switch_scale_rlr","TRUE")
    updateNumericInput(session,"log_landa",value = 2)
    updateRadioSwitch(session,"permitir_landa","FALSE")
    
    #---------------knn-----------------
    
    isolate(datos <- new.data$datos.train)
    if(!is.null(datos)){
      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.numericas(datos))))
      updateNumericInput(session, "kmax_knn", value = round(sqrt(nrow(datos))))
      updateNumericInput(session, "mtry.rf.pred", value = round(sqrt(ncol(datos) -1)))
    }
    
  }
  
  
  # Update the different tables in the "shiny" application
  # update_table_pn <- function(tablas = c("contentsPred", "contentsPred2")){
  #   if("contentsPred2" %in% tablas){
  #     output$contentsPred <- render_table_data(datos.aprendizaje.completos,editable = F,
  #                                              scrollY = "25vh", server = F)
  #   }
  #   if("contentsPred2" %in% tablas){
  #     output$contentsPred2 <- render_table_data(datos.aprendizaje.completos,editable = F,
  #                                               scrollY = "25vh", server = F)
  #   }
  #   if("contentsPred3" %in% tablas){
  #     output$contentsPred3 <- render_table_data(datos.prueba.completos,editable = F,
  #                                               scrollY = "25vh", server = T)
  #   }
  # }
  
  
  # Updates neural network layers of new individuals
  update_nn_layers_pn <- function(){
    if(!is.null(input$cant.capas.nn.pred)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn.pred) {
          shinyjs::show(paste0("nn.cap.pred.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.pred.", i))
        }
      }
    }
  }
  
  #update_nn_layers_pn()
  
  # When the number of neural network layers changes.
  observeEvent(c(input$cant.capas.nn.pred), {
    update_nn_layers_pn()
  })
  
  
  
  # Download the data with the prediction
  # output$downloaDatosPred <- downloadHandler(
  #   filename = function() {
  #     input$file3$name
  #   },
  #   content = function(file) {
  #     if(!is.null(predic.nuevos)){
  #       write.csv(new_col(datos.prueba.completos, variable.predecir.pn, predic.nuevos), file, row.names = input$rownameNPred2)
  #     }
  #   }
  # )
  
  
  
  # When the number of components changes.
  observeEvent(input$permitir.ncomp.pred, {
    if (input$permitir.ncomp.pred) {
      shinyjs::enable("ncomp.rd.pred")
    } else {
      shinyjs::disable("ncomp.rd.pred")
    }
  })
  
  # When user press enable or disable the lambda
  observeEvent(input$permitir_landa, {
    if (as.logical(input$permitir_landa)) {
      shinyjs::enable("log_landa")
    } else {
      shinyjs::disable("log_landa")
    }
  })
  
  reset.data <- function(){
    isolate({
      new.data$originales.train <- NULL
      new.data$datos.train      <- NULL
      new.data$variable.predecir <- NULL
      new.data$nuevos <- NULL
      modelos$new.data$modelo <- NULL
      modelos$new.data$prediccion <- NULL
    })
  }
  
  
  reset.next.btns <- function(){
    shinyjs::hide("btn_next3",anim = TRUE)
    shinyjs::hide("btn_next4",anim = TRUE)
  }
  
  
  #' Load Button Function 1
  observeEvent(input$loadButtonNPred, {
    tryCatch({
      shinyjs::hide("btn_next1",anim = TRUE)
      
      reset.data()
      
      rowname    <- input$rownameNPred
      ruta       <- input$file2
      sep        <- input$sepNPred
      dec        <- input$decNPred
      encabezado <- input$headerNPred
      deleteNA   <- input$deleteNAnPred
      
      new.data$originales.train <- carga.datos(
        rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
      
      if(ncol(new.data$originales.train) <= 1) {
        showNotification("ERROR: Check Separators", duration = 10, type = "error")
        new.data$originales.train <- NULL
      } else {
        #Todo correcto
        new.data$datos.train <- new.data$originales.train
        default.values.inputs()
        reset.next.btns()
        shinyjs::show("btn_next1",anim = TRUE)
      }
    }, error = function(e) {
      reset.data()
      showNotification(paste0(tr("errorSeg"), e), type = "error")
    })
    
  },ignoreInit = TRUE)
  
  
  updateDataTable1_2 <- reactive({
    datos  <- new.data$datos.train
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  })
  
  
  #' Update data on table1
  output$tabladatos1 <- DT::renderDataTable({
    #Se necesita por problema con la reactividad
    input$btn_prev1
    updateDataTable1_2()
  }, server = T)
  
  #' Update data on table2
  output$tabladatos2 <- DT::renderDataTable({
    input$btn_next1
    updateDataTable1_2()
  }, server = T)
  
  
  #' Update Transform Table1
  output$transData1 <- renderUI({
    datos  <- new.data$originales.train
    idioma <- updateData$idioma
    
    res <- list(fluidRow(
      column(4, tags$span(tags$b("Variable"))),
      column(5, tags$b(tr("tipo", idioma))),
      column(3, tags$b(tr("activa", idioma))),
    ), hr(style = paste0("margin-top: 10px; margin-bottom: 10px;", 
                         "border-top: 1px solid black;")))
    
    if(!is.null(datos) && ncol(datos) > 0) {
      res <- list(res, lapply(colnames(datos), function(x) {
        list(fluidRow(
          column(4, tags$span(x)),
          column(5, selectInputTrans(datos, x, session, idioma)),
          column(3, tags$input(type = "checkbox", id = ns(paste0("del", x)), 
                               checked = T))
        ), hr(style = "margin-top: 10px; margin-bottom: 10px"))
      }))
    }
    
    res <- tags$div(
      style = "height: 40vh; overflow-y: scroll;",
      do.call(tagList, res)
    )
    return(res)
  })
  
  
  #' Transform Button Function
  observeEvent(input$transButton1, {
    
    datos  <- new.data$originales.train
    cod = ""
    
    for (var in colnames(datos)) {
      if(!input[[paste0("del", var)]]) {
        datos[, var] <- NULL
        cod <- paste0(cod, "datos[['", var, "']] <- NULL\n")
        
      } else {
        if(input[[paste0("sel", var)]] == "categorico" &
           class(datos[, var]) %in% c("numeric","integer")) {
          datos[, var] <- as.factor(datos[, var])
          cod <- paste0(cod, code.trans(var, "categorico"))
        }
        if(input[[paste0("sel", var)]] == "numerico" &
           !(class(datos[, var]) %in% c("numeric","integer"))) {
          datos[, var] <- as.numeric(datos[, var])
          cod <- paste0(cod, code.trans(var, "numerico"))
        }
        if(input[[paste0("sel", var)]] == "disyuntivo") {
          datos <- datos.disyuntivos(datos, var)
          datos[, var] <- NULL
          cod <- paste0(cod, code.trans(var, "disyuntivo"))
        }
      }
    }
    
    modelos$new.data$modelo <- NULL
    modelos$new.data$prediccion <- NULL
    new.data$datos.train <- datos
    default.values.inputs()
    reset.next.btns()
  }, ignoreInit = TRUE)
  
  
  
  #Update model tab
  output$txtPredNuevos <- renderPrint({
    tryCatch({
      if(!is.null(modelos$new.data$modelo)){
        print(modelos$new.data$modelo)
      }
      else{
        cat(tr("noModel", updateData$idioma))
        updateAceEditor(session, "fieldPredNuevos", value = "")
      }
    }, error = function(e){
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
      NULL
    } )
  })
  
  
  # When the user presses the generate model button
  observeEvent(input$PredNuevosBttnModelo,{
    
    tryCatch({
      
      shinyjs::hide(id = "btn_next4", anim = T)
      
      variable.predecir <- input$sel.predic.var.nuevos
      new.data$variable.predecir <- variable.predecir
      datos.aprendizaje <- new.data$datos.train
      modelo.seleccionado  <- input$selectModelsPred
      
      gen.code <- ""
      
      modelos$new.data$modelo <- switch(modelo.seleccionado,
                                        rl   = {
                                          gen.code <- codeRl(variable.predecir)
                                          rl_model(datos.aprendizaje,variable.predecir)
                                        },
                                        rlr  = {
                                          alpha <- as.numeric(input$alpha.rlr)
                                          standardize <- as.logical(input$switch_scale_rlr)
                                          gen.code <- codeRlr(variable.predecir,alpha,standardize)
                                          rlr_model(data = datos.aprendizaje, variable.pred = variable.predecir,
                                                    alpha = alpha, standardize = standardize)
                                        },
                                        knn =  kkn_model(data = "datos.aprendizaje.completos",
                                                         variable.pred = variable.predecir.pn,
                                                         scale = input$switch.scale.knn.pred,
                                                         kmax = input$kmax.knn.pred,
                                                         kernel = input$kernel.knn.pred,
                                                         model.var = "modelo.nuevos",
                                                         distance = input$distance.knn.pred),
                                        dt  = dt_model(data = "datos.aprendizaje.completos",
                                                       variable.pred = variable.predecir.pn,
                                                       model.var = "modelo.nuevos",
                                                       minsplit = input$minsplit.dt.pred,
                                                       maxdepth = input$maxdepth.dt.pred),
                                        rf  = rf_model(data = "datos.aprendizaje.completos",
                                                       variable.pred = variable.predecir.pn,
                                                       model.var = "modelo.nuevos",
                                                       ntree = input$ntree.rf.pred,
                                                       mtry = input$mtry.rf.pred),
                                        boosting = boosting_model(data = "datos.aprendizaje.completos",
                                                                  variable.pred = variable.predecir.pn,
                                                                  model.var = "modelo.nuevos",
                                                                  n.trees = input$iter.boosting.pred,
                                                                  distribution = input$tipo.boosting.pred,
                                                                  shrinkage = input$shrinkage.boosting.pre),
                                        svm = svm_model(data = "datos.aprendizaje.completos",
                                                        variable.pred = variable.predecir.pn,
                                                        model.var = "modelo.nuevos",
                                                        scale = input$switch.scale.svm.pred,
                                                        kernel = input$kernel.svm.pred),
                                        rd = rd_model(data = "datos.aprendizaje.completos",
                                                      variable.pred = variable.predecir.pn,
                                                      model.var = "modelo.nuevos",
                                                      n.comp = "n.comp.rd.np",
                                                      mode = input$mode.rd.pred,
                                                      scale = input$switch.scale.rd.pred),
                                        nn = nn_model(data = "datos.aprendizaje.completos",
                                                      variable.pred = variable.predecir.pn,
                                                      model.var = "modelo.nuevos",
                                                      mean.var = "mean.nn.np",
                                                      sd.var = "sd.nn.np",
                                                      threshold = input$threshold.nn.pred,
                                                      stepmax = input$stepmax.nn.pred,
                                                      cant.hidden = input$cant.capas.nn.pred,
                                                      input$nn.cap.pred.1,input$nn.cap.pred.2,
                                                      input$nn.cap.pred.3,input$nn.cap.pred.4,
                                                      input$nn.cap.pred.5,input$nn.cap.pred.6,
                                                      input$nn.cap.pred.7,input$nn.cap.pred.8,
                                                      input$nn.cap.pred.9,input$nn.cap.pred.10))
      
      
      updateAceEditor(session, "fieldPredNuevos", value = gen.code)
      
      
      # if( (input$selectModelsPred == "boosting" &&
      #      !is.null(calibrate_boosting(datos.aprendizaje.completos)) ) ||
      #     input$selectModelsPred != "boosting" ){
      #   exe(codigo)
      #   update_model_text_pn(codigo)
      # }else{
      #   showNotification(tr("ErrorBsize"), duration = 10, type = "error")
      # }
      
      shinyjs::show(id = "btn_next3", anim = T)
    },
    error =  function(e){
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
    },
    warning = function(w){
      if(input$selectModelsPred == "nn"){
        showNotification(paste0(tr("nnWar")," (NN-01) : ",w), duration = 10, type = "warning")
      }
    })
  }, ignoreInit = TRUE)
  
  
  
  #' Load Button Function 2
  observeEvent(input$loadButtonNPred2, {
    tryCatch({
      
      shinyjs::hide("btn_next4",anim = TRUE)
      
      rowname    <- input$rownameNPred2
      ruta       <- input$file3
      sep        <- input$sep.nPred2
      dec        <- input$dec.nPred2
      encabezado <- input$headerNPred2
      deleteNA   <- input$deleteNAnPred2
      
      new.data$nuevos <- carga.datos(
        rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
      
      if(ncol(new.data$nuevos) <= 1) {
        showNotification("ERROR: Check Separators", duration = 10, type = "error")
        new.data$nuevos <- NULL
      } else {
        #Todo correcto
        shinyjs::show("btn_next4",anim = TRUE)
      }
    }, error = function(e) {
      showNotification(paste0(tr("errorSeg"), e), type = "error")
    })
    
  },ignoreInit = TRUE)
  
  
  
  #' Update data on table3
  output$tabladatos3 <- DT::renderDataTable({
    datos  <- new.data$nuevos
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)
  
  
  
  #Update prediction tab
  observeEvent(modelos$new.data$prediccion,{
    tryCatch({
      prediccion <- modelos$new.data$prediccion
      if(!is.null(prediccion)){
        datos.nuevos.pred <- new.data$nuevos
        datos.nuevos.pred[, new.data$variable.predecir] <- prediccion
        output$PrediTablePN <- render_table_data(datos.nuevos.pred,editable = F,
                                                 scrollY = "40vh",server = T)
      }
      else{
        output$PrediTablePN <- DT::renderDT(DT::datatable(data.frame()))
        updateAceEditor(session, "fieldCodePredPN", value = "")
      }
    }, error = function(e){
      showNotification(paste0("Error :", e), duration = 10, type = "error")
      output$PrediTablePN <- DT::renderDT(DT::datatable(data.frame()))
    })
  })
  
  
  # When the user enters the prediction panel
  observeEvent(input$predecirPromidat, {
    
    tryCatch({
      
      datos.prueba <- new.data$nuevos
      
      if(!is.null(new.data$nuevos)){
        modelo <- modelos$new.data$modelo
        
        if(!is.null(modelo)){
          modelo.seleccionado  <- input$selectModelsPred
          pred.code <- ""
          
          modelos$new.data$prediccion <- switch(modelo.seleccionado,
                                                rl  =  {
                                                  pred.code <- codeRlPred("rl.model")
                                                  rl_prediction(modelo, datos.prueba)
                                                },
                                                rlr =  {
                                                  pred.code <- codeRlrPred("rlr.model", variab)
                                                },rlr_prediction(data.a = "datos.aprendizaje.completos",
                                                                      data.p = 'datos.prueba.completos',
                                                                      variable.pred = variable.predecir.pn,
                                                                      model.var = 'modelo.nuevos',
                                                                      pred.var = 'predic.nuevos',
                                                                      lambda = if(input$permitir.landa.pred){ifelse(is.na(input$landa.pred),NULL,input$landa.pred)}else{NULL},
                                                                      cv.var = "cv.glm.nuevos"),
                                                knn =  kkn_prediction(data = 'datos.prueba.completos',
                                                                      variable.pred = variable.predecir.pn,
                                                                      model.var = 'modelo.nuevos',
                                                                      pred.var  = 'predic.nuevos'),
                                                dt  = dt_prediction(data = "datos.prueba.completos",
                                                                    model.var = "modelo.nuevos",
                                                                    pred.var = "predic.nuevos"),
                                                rf  = rf_prediction(data = "datos.prueba.completos",
                                                                    variable.pred = variable.predecir.pn,
                                                                    model.var = "modelo.nuevos",
                                                                    pred.var = "predic.nuevos"),
                                                boosting = boosting_prediction(data = "datos.prueba.completos",
                                                                               variable.pred = variable.predecir.pn,
                                                                               model.var = "modelo.nuevos",
                                                                               pred.var = "predic.nuevos",
                                                                               n.trees = input$iter.boosting.pred),
                                                svm = svm_prediction(data = "datos.prueba.completos",
                                                                     variable.pred = variable.predecir.pn,
                                                                     model.var = "modelo.nuevos",
                                                                     pred.var = "predic.nuevos"),
                                                rd  =  rd_prediction(data = "datos.prueba.completos",
                                                                     model.var = "modelo.nuevos",
                                                                     pred.var = "predic.nuevos",
                                                                     n.comp = "n.comp.rd.np",
                                                                     ncomp = if(input$permitir.ncomp.pred){input$ncomp.rd.pred}else{NULL}),
                                                nn = nn_prediction(data = "datos.prueba.completos",
                                                                   variable.pred = variable.predecir.pn,
                                                                   model.var = "modelo.nuevos",
                                                                   pred.var = "predic.nuevos",
                                                                   mean.var = "mean.nn.np",
                                                                   sd.var = "sd.nn.np"))
          
          updateAceEditor(session, "fieldCodePredPN", value = pred.code)
          
        }else{
          showNotification(paste0("Error :", tr("ErrorModelo")), duration = 10, type = "error")
        }
      }else{
        showNotification(paste0("Error :", tr("ErrorDatosPN")), duration = 10, type = "error")
      }
    },error =  function(e){
      showNotification(paste0("Error :", e), duration = 10, type = "error")
    })
  })
  
}

## To be copied in the UI
# mod_new_data_predictions_ui("new_data_predictions_ui_1")

## To be copied in the server
# callModule(mod_new_data_predictions_server, "new_data_predictions_ui_1")

