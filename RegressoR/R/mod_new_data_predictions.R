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
  
  show.data.pred <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                        DT::DTOutput(ns('contentsPred')), type = 7, color = "#CBB051")
  
  show.data.pred2 <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                         DT::DTOutput(ns('contentsPred2')), type = 7, color = "#CBB051")
  
  show.data.pred3 <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                         DT::DTOutput(ns('contentsPred3')), type = 7, color = "#CBB051")
  
  # Loading and transforming data
  
  data.upload.panel.pred <- div(id = ns("seccion1"),fluidRow(column(width = 11, 
                                    tabBox(width = 12,
                                          tabPanel(title =labelInput("cargarDatos"), fluidRow(column(width = 5,
                                                     checkboxInput(ns('headerNPred'), labelInput("header"), TRUE),
                                                     checkboxInput(ns('rownameNPred'), labelInput("Rownames"), TRUE),
                                                     radioButtons(ns('sepNPred'), labelInput("separador"), inline = T, choiceValues = c(';', ',', '\t'), choiceNames = c(';', ',', 'TAB')),
                                                     radioButtons(ns('decNPred'),labelInput("separadordec"), inline = T,choiceValues = c(',', '.'), choiceNames = c(',', '.')),
                                                     radioSwitch(ns("deleteNAnPred"), "eliminana", c("eliminar", "imputar")),
                                                     fileInput(ns('file2'), label = labelInput("cargarchivo"), placeholder = "", buttonLabel =  labelInput("subir"), width = "100%",
                                                               accept = c('text/csv', '.csv')),
                                                     actionButton(ns("loadButtonNPred"), labelInput("cargar"), width = "100%")),
                                              column(width = 7, show.data.pred))))),
                                    
                                     column(width = 1, actionButton(inputId = ns("btn_next1"),
                                                                    label = NULL, icon = icon("forward"), style = btn_style_hidden) )))
  
  
  tansform.data.panel <-   div(id = ns("seccion2"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev1"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 10,tabBox(
      width = 12,
      tabPanel(title = labelInput("transDatos"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
               fluidRow(column(width = 5,
                               DT::dataTableOutput(ns('transDataPredN')),
                               br(),br(),
                               actionButton(ns("transButtonPredN"), labelInput("aplicar"), width = "100%")),
                        column(width = 7, show.data.pred2))))),
    
    column(width = 1, actionButton(inputId = ns("btn_next2"),
                                   label = NULL, icon = icon("forward"), style = btn_style) )))
  
  
  
  data.upload.panel.pred2 <- div(id = ns("seccion3"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev2"),
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
                        column(width = 7, show.data.pred3))))),
    column(width = 1, actionButton(inputId = ns("btn_next3"),
                                   label = NULL, icon = icon("forward"), style = btn_style) )))
  
  # Model Options
  
  options.rl.pred <- list() # Vacio
  
  options.rlr.pred <- fluidRow(column(selectInput(inputId = ns("alpha.rlr.pred"), label = labelInput("selectAlg"),selected = 1,
                                                  choices = list("Ridge" = 0, "Lasso" = 1)),width = 3),
                               column(br(), switchInput(inputId = ns("switch.scale.rlr.pred"), onStatus = "success", offStatus = "danger", value = T,
                                                        label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=3),
                               column(id = ns("colManualLanda.pred"),width = 3, numericInput(ns("landa.pred"), labelInput("landa"),value = 2, min = 0, "NULL", width = "100%")), br(),
                               column(width = 3, switchInput(inputId = ns("permitir.landa.pred"), onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                             label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%")))
  
  options.rd.pred <-  fluidRow(column(selectInput(inputId = ns("mode.rd.pred"), label = labelInput("selectAlg"),selected = 0,
                                                  choices = list("ACP" = 0, "MCP" = 1)),width = 3),
                               column(br(), switchInput(inputId = ns("switch.scale.rd.pred"), onStatus = "success", offStatus = "danger", value = T,
                                                        label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=3),
                               column(id = ns("colManualCom.pred"),width = 3, numericInput(ns("ncomp.rd.pred"), labelInput("ncomp"),value = 2, min = 0, "NULL", width = "100%")), br(),
                               column(width = 3, switchInput(inputId = ns("permitir.ncomp.pred"), onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                             label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%")))
  
  options.knn.pred <- fluidRow(column(width = 3, br() , switchInput(inputId = ns("switch.scale.knn.pred"), onStatus = "success", offStatus = "danger", value = T,
                                                                    label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%", width = "100%")),
                               column(width = 3, numericInput(ns("kmax.knn.pred"), labelInput("kmax"), min = 1,step = 1, value = 7,width="100%")),
                               column(width = 3, selectInput(inputId = ns("kernel.knn.pred"), label = labelInput("selkernel") ,selected = 1, width="100%",
                                                             choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                                          "triweight", "cos","inv","gaussian"))),
                               column(width = 3,numericInput(ns("distance.knn.pred"), labelInput("distknn"), min = 1,step = 1, value = 2) ))
  
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
                                                      min = 0, step = 0.01, value = 0.05), width = 4),
                                  column(numericInput(ns("stepmax.nn.pred"),labelInput("stepmax"),
                                                      min = 100, step = 100, value = 5000), width = 4),
                                  column(sliderInput(inputId = ns("cant.capas.nn.pred"), min = 1, max = 10,
                                                     label = labelInput("selectCapas"), value = 10), width = 4)),
                         fluidRow(lapply(1:10, function(i) tags$span(numericInput(ns(paste0("nn.cap.pred.",i)), NULL,
                                                                                  min = 1, step = 1, value = 2),
                                                                     class = "mini-numeric-select"))))
  
  options.model <- list(selectInput(inputId = ns("sel.predic.var.nuevos"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"),
                        radioGroupButtons(ns("selectModelsPred"), labelInput("selectMod"), 
                                          list("<span data-id=\"rll\"></span>" = "rl",
                                               "<span data-id=\"rlr\"></span>" = "rlr",
                                               "<span data-id=\"rd\"></span>" = "rd",
                                               "<span data-id=\"knnl\"></span>" = "knn",
                                               "<span data-id=\"dtl\"></span>" = "dt",
                                               "<span data-id=\"rfl\"></span>" = "rf",
                                               "<span data-id=\"bl\"></span>" = "boosting",
                                               "<span data-id=\"svml\"></span>" = "svm",
                                               "<span data-id=\"rd\"></span>" = "rd",
                                               "<span data-id=\"nn\"></span>" = "nn"),
                                          size = "sm", status = "primary",individual = FALSE, justified = FALSE, selected = "knn",
                                          checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                           no = icon("remove", lib = "glyphicon"))))
  
  create.pred.model.panel <- div(id = ns("seccion4"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev3"),
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
               verbatimTextOutput(ns("txtPredNuevos")),
               actionButton(ns("PredNuevosBttnModelo"), labelInput("generarM"), 
                            width  = "100%", style = "background-color:#CBB051;color:#fff;margin-top:9px;")))),
    column(width = 1, actionButton(inputId = ns("btn_next4"),
                                   label = NULL, icon = icon("forward"), style = btn_style) )
  ))
  
  
  tabs.models  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                              tabs.content = list(list(aceEditor(ns("fieldPredNuevos"), mode = "r", theme = "monokai", value = "", height = "20vh", readOnly = F))))
  
  tabs.models2  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                               tabs.content = list(aceEditor(ns("fieldCodePredPN"), mode = "r", theme = "monokai",
                                                             value = "", height = "20vh", readOnly = F, autoComplete = "enabled")))
  
  prediccion.pred.panel <- div(id = ns("seccion5"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev4"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 11,tabBox(
      width = 12,
      tabPanel(title = labelInput("predicnuevos"), value = "predicModelo",
               DT::dataTableOutput(ns("PrediTablePN")),
               hr(),
               downloadButton(ns("downloaDatosPred"), labelInput("descargar"), style = "width:100%"),
               actionButton(ns("predecirPromidat"), "preditc",style="display:none;"))))))
  
  page.new.predictions <- tabItem(tabName = "predNuevos",
                                  tabBox(id = ns("BoxModelo"), width = NULL, height ="80%",
                                         data.upload.panel.pred,
                                         tansform.data.panel,
                                         create.pred.model.panel,
                                         data.upload.panel.pred2,
                                         prediccion.pred.panel,
                                         conditionalPanel(condition =  "input.BoxModelo == 'crearModelo'", tabs.models, ns = ns),
                                         conditionalPanel(condition =  "input.BoxModelo == 'predicModelo'", tabs.models2, ns = ns)))
  
  
  
  tagList(
    data.upload.panel.pred,
    tansform.data.panel,
    create.pred.model.panel,
    data.upload.panel.pred2,
    prediccion.pred.panel
    #page.new.predictions
  )
}
    
#' new_data_predictions Server Function
#'
#' @noRd 
mod_new_data_predictions_server <- function(input, output, session,updateData,updatePlot){
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
  })
  
  observeEvent(input$btn_prev4,{
    shinyjs::show("seccion4",anim = T)
    shinyjs::hide("seccion5",anim = T)
  })
 
  
  # Update the different tables in the "shiny" application
  update_table_pn <- function(tablas = c("contentsPred", "contentsPred2")){
    if("contentsPred2" %in% tablas){
      output$contentsPred <- render_table_data(datos.aprendizaje.completos,editable = F,
                                               scrollY = "25vh", server = F)
    }
    if("contentsPred2" %in% tablas){
      output$contentsPred2 <- render_table_data(datos.aprendizaje.completos,editable = F,
                                                scrollY = "25vh", server = F)
    }
    if("contentsPred3" %in% tablas){
      output$contentsPred3 <- render_table_data(datos.prueba.completos,editable = F,
                                                scrollY = "25vh", server = T)
    }
  }
  
  # Update the model text for prediction of new individuals
  update_model_text_pn <- function(codigo){
    updateAceEditor(session, "fieldPredNuevos", value = codigo)
    if(is.null(modelo.nuevos)){
      output$txtPredNuevos <- renderPrint(invisible(NULL))
    }else{
      output$txtPredNuevos <- renderPrint(print(modelo.nuevos))
    }
  }
  
  # Update the prediction table of new individuals
  update_pred_pn <- function(codigo){
    updateAceEditor(session, "fieldCodePredPN", value = codigo)
    if(!is.null(predic.nuevos)){
      datos.aux.prueba <- new_col(datos.prueba.completos, variable.predecir.pn, predic.nuevos)
      output$PrediTablePN <- render_table_data(datos.aux.prueba,editable = F,
                                               scrollY = "25vh",server = T)
    }else{
      output$PrediTablePN <- DT::renderDT(DT::datatable(data.frame()))
    }
  }
  
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
  
  # Download the data with the prediction
  output$downloaDatosPred <- downloadHandler(
    filename = function() {
      input$file3$name
    },
    content = function(file) {
      if(!is.null(predic.nuevos)){
        write.csv(new_col(datos.prueba.completos, variable.predecir.pn, predic.nuevos), file, row.names = input$rownameNPred2)
      }
    }
  )
  
  # When the number of neural network layers changes.
  observeEvent(c(input$cant.capas.nn.pred), {
    update_nn_layers_pn()
  })
  
  # When the number of components changes.
  observeEvent(input$permitir.ncomp.pred, {
    if (input$permitir.ncomp.pred) {
      shinyjs::enable("ncomp.rd.pred")
    } else {
      shinyjs::disable("ncomp.rd.pred")
    }
  })
  
  # When allowing the lambda changes
  observeEvent(input$permitir.landa.pred, {
    if (input$permitir.landa.pred) {
      shinyjs::enable("landa.pred")
    } else {
      shinyjs::disable("landa.pred")
    }
  })
  
  
  # When learning data is loaded
  observeEvent(input$loadButtonNPred,{
    shinyjs::hide("btn_next1",anim = TRUE)
    codigo.carga <- code_load(row.names = input$rownameNPred,
                              path = input$file2$datapath,
                              sep = input$sepNPred,
                              sep.dec = input$decNPred,
                              header = input$headerNPred,
                              d.o = "datos.originales.completos",
                              d = "datos.aprendizaje.completos")
    
    tryCatch({
      isolate(exe(codigo.carga))
      if(ncol(datos.originales.completos) <= 1) {
        showNotification(translate("errorSeg"), duration = 10, type = "error")
        return(NULL)
      }
      codigo.na <- ""
      codigo.na <- paste0(code_NA(deleteNA = input$deleteNAnPred, d.o = "datos.originales.completos"), 
                          "\n", "datos.aprendizaje.completos <- datos.originales.completos")
      isolate(exe(codigo.na))
      
      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames_empty(var_numerical(datos.aprendizaje.completos))))
      updateNumericInput(session, "kmax.knn.pred", value = round(sqrt(nrow(datos.aprendizaje.completos))))
      updateNumericInput(session, "mtry.rf.pred", value = round(sqrt(ncol(datos.aprendizaje.completos) -1)))
      shinyjs::show("btn_next1",anim = TRUE)
    },
    error = function(e) {
      showNotification(paste0("Error:", e), duration = 10, type = "error")
      datos.aprendizaje.completos <<- NULL
      datos.originales.completos <<- NULL
      return(NULL)
    })
    
    modelo.nuevos <<- NULL
    predic.nuevos <<- NULL
    update_pred_pn("")
    update_model_text_pn("")
    update_table_pn()
  })
  
  # Show the select box of the panel to transform data (eventReactive)
  update_trans_pn <- eventReactive(c(input$loadButtonNPred), {
    contadorPN <<- contadorPN + 1
    if (!is.null(datos.aprendizaje.completos) && ncol(datos.aprendizaje.completos) > 0) {
      res <- data.frame(Variables = colnames(datos.aprendizaje.completos),
                        Tipo = c(1:ncol(datos.aprendizaje.completos)),
                        Activa = c(1:ncol(datos.aprendizaje.completos)))
      res$Tipo <- sapply(colnames(datos.aprendizaje.completos), function(i) paste0(
        '<select id="', ns('Predsel'), i, contadorPN, '"> <option value="categorico">',translate("categorico"),'</option>',
        '<option value="numerico" ', ifelse(class(datos.aprendizaje.completos[, i]) %in% c("numeric", "integer"),' selected="selected"', ""),'>', translate("numerico"),'</option>',
        '<option value="disyuntivo">',translate("disyuntivo"),'</option> </select>'
      ))
      res$Activa <- sapply(colnames(datos.aprendizaje.completos), function(i) paste0('<input type="checkbox" id="',ns('Predbox'), i, contadorPN, '" checked/>'))
      update_nn_layers_pn()
    } else {
      res <- as.data.frame(NULL)
      showNotification(translate("tieneCData"), duration = 10, type = "error")
    }
    return(res)
  })
  
  # Show the select box of the panel to transform data
  output$transDataPredN <- DT::renderDataTable(update_trans_pn(),
                                               escape = FALSE, selection = "none", server = FALSE,
                                               options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = "35vh"), rownames = F,
                                               callback = JS("table.rows().every(function(i, tab, row) {
                                                        var $this = $(this.node());
                                                        $this.attr('id', this.data()[0]);
                                                        $this.addClass('shiny-input-checkbox');});
                                                        Shiny.unbindAll(table.table().node());
                                                        Shiny.bindAll(table.table().node());"))
  
  # Use and show the codes to transform the data
  transform_data_pn <- function() {
    var.noactivas <- c()
    code.res <- "datos.aprendizaje.completos <- datos.originales.completos \n"
    for (var in colnames(datos.originales.completos)) {
      if (input[[paste0("Predbox", var, contadorPN)]]) {
        if (input[[paste0("Predsel", var, contadorPN)]] == "categorico" & class(datos.originales.completos[, var]) %in% c("numeric", "integer")) {
          code.res <- paste0(code.res, code_transf(var, "categorico", d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
        if (input[[paste0("Predsel", var, contadorPN)]] == "numerico" & !(class(datos.originales.completos[, var]) %in% c("numeric", "integer"))) {
          code.res <- paste0(code.res, code_transf(var, "numerico",  d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
        if (input[[paste0("Predsel", var, contadorPN)]] == "disyuntivo") {
          code.res <- paste0(code.res, code_transf(var, "disyuntivo", d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }
    
    isolate(exe(code.res))
    if (length(var.noactivas) > 0) {
      des <- code_deactivate(var.noactivas,"datos.aprendizaje.completos")
      isolate(exe(des))
      code.res <- paste0(code.res, "\n", des)
    }
    code.res <- paste0(code.res, "\n")
    return(code.res)
  }
  
  # When the user enters the prediction panel
  observeEvent(input$predecirPromidat, {
    if(!is.null(datos.prueba.completos)){
      if(exists("modelo.nuevos") && !is.null(modelo.nuevos)){
        codigo <- switch(modelo.seleccionado.pn,
                         rl  =  rl_prediction(data = 'datos.prueba.completos', 
                                              model.var = 'modelo.nuevos', 
                                              pred.var = 'predic.nuevos'),
                         rlr =  rlr_prediction(data.a = "datos.aprendizaje.completos",
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
        tryCatch({
          exe(codigo)
          update_pred_pn(codigo)
        },error =  function(e){
          showNotification(paste0("Error :", e), duration = 10, type = "error")
        })
      }else{
        showNotification(paste0("Error :", translate("ErrorModelo")), duration = 10, type = "error")
      }
    }else{
      showNotification(paste0("Error :", translate("ErrorDatosPN")), duration = 10, type = "error")
    }
  })
  
  # When the data transform button is pressed
  observeEvent(input$transButtonPredN, {
    code.trans.pn <<- transform_data_pn()
    updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames_empty(var_numerical(datos.aprendizaje.completos))))
    updateNumericInput(session, "mtry.rf.pred", value = round(sqrt(ncol(datos.aprendizaje.completos) -1)))
    
    modelo.nuevos <<- NULL
    predic.nuevos <<- NULL
    update_pred_pn("")
    
    update_model_text_pn("")
    update_table_pn()
  })
  
  # When the user presses the generate model button
  observeEvent(input$PredNuevosBttnModelo,{
    variable.predecir.pn <<- input$sel.predic.var.nuevos
    modelo.seleccionado.pn  <<- input$selectModelsPred
    
    codigo <- switch(input$selectModelsPred,
                     rl   = rl_model(data = "datos.aprendizaje.completos", 
                                     variable.pred = variable.predecir.pn, 
                                     model.var = "modelo.nuevos"),
                     rlr  = rlr_model(data = "datos.aprendizaje.completos",
                                      variable.pred = variable.predecir.pn,
                                      model.var = "modelo.nuevos",
                                      cv.var = "cv.glm.nuevos",
                                      alpha = input$alpha.rlr.pred,
                                      standardize = input$switch.scale.rlr.pred),
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
    
    modelo.nuevos <<- NULL
    predic.nuevos <<- NULL
    update_pred_pn("")
    
    tryCatch({
      if( (input$selectModelsPred == "boosting" &&
           !is.null(calibrate_boosting(datos.aprendizaje.completos)) ) ||
          input$selectModelsPred != "boosting" ){
        exe(codigo)
        update_model_text_pn(codigo)
      }else{
        showNotification(translate("ErrorBsize"), duration = 15, type = "error")
      }
    },
    error =  function(e){
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
    },
    warning = function(w){
      if(input$selectModelsPred == "nn"){
        showNotification(paste0(translate("nnWar")," (NN-01) : ",w), duration = 20, type = "warning")
      }
    })
  })
  
  # When the user loads the data
  observeEvent(input$loadButtonNPred2,{
    codigo.carga <- code_load(row.names = input$rownameNPred2,
                              path = input$file3$datapath,
                              sep = input$sep.nPred2,
                              sep.dec = input$dec.nPred2,
                              header  = input$headerNPred2,
                              d.o = "datos.prueba.completos",
                              d = "datos.prueba.completos")
    
    tryCatch({
      isolate(exe(codigo.carga))
      codigo.na <- ""
      codigo.na <- paste0(code_NA(deleteNA = input$deleteNAnPred2,
                                  d.o = paste0("datos.prueba.completos")))
      datos.prueba.completos[,variable.predecir.pn] <<- NULL
      validate_pn_data(datos.originales.completos, datos.prueba.completos, variable.predecir.pn)
      isolate(exe( codigo.na))
      datos.prueba.completos[,variable.predecir.pn] <<- NA_real_
      code.trans.pn <<- gsub("datos.originales.completos", "datos.prueba.completos", code.trans.pn)
      code.trans.pn <<- gsub("datos.aprendizaje.completos", "datos.prueba.completos", code.trans.pn)
      exe(code.trans.pn)
      if(ncol(datos.prueba.completos) <= 1) {
        showNotification(translate("errorSeg"), duration = 10, type = "error")
        return(NULL)
      }
      update_table_pn("contentsPred3")
    },
    error = function(e) {
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
      datos.prueba.completos <<- NULL
      predic.nuevos <<- NULL
      return(NULL)
    })
  })
}
    
## To be copied in the UI
# mod_new_data_predictions_ui("new_data_predictions_ui_1")
    
## To be copied in the server
# callModule(mod_new_data_predictions_server, "new_data_predictions_ui_1")
 
