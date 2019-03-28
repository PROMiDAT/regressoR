


# The server for RegressoR
shinyServer(function(input, output, session) {
  
  # The following variables belong to the server environment
  # variable.predecir = The name of the variable to predict
  # datos = The full dataset
  # datos.prueba = The test dataset partition
  # datos.aprendizaje = The learning dataset partition
  # real.val = The values of the variable to predict (test data)
  
  # SERVER UTILITY FUNCTIONS ----------------------------------------------------------------------------------------------
  
  # Wrapper of regressoR::error.variables to set the language
  error.variables <- function(num = T){
    regressoR::error.variables(num, input$idioma)
  }
  
  # Wrapper of regressoR::translate to set the language
  translate <- function(labelid){
    regressoR::translate(labelid, input$idioma)
  }

  # Wrapper of regressoR::render_table_data to set the language
  render_table_data <- function(data, editable = TRUE, dom = "frtip", pageLength = 10, scrollY = "27vh", server = T, language = "es"){
    regressoR::render_table_data(data, editable, dom, pageLength, scrollY, server, input$idioma)
  }
  
  # Wrapper of regressoR::exe to set the environment
  exe <- function(...){
    regressoR::exe(..., envir = parent.frame())
  }
  
  # Wrapper of regressoR::models_mode to set the list of values and the language
  models_mode <- function(){
    regressoR::models_mode(IndicesM, input$idioma)
  }
  
  # Wrapper of regressoR::comparative.table to set the list of values and the language
  comparative.table <- function(sel){
    regressoR::comparative.table(sel, IndicesM, input$idioma)
  }
  
  #
  validate_pn_data <- function(){
    regressoR::validate_pn_data(datos.originales.completos, datos.prueba.completos, variable.predecir.pn, input$idioma)
  }
  
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

  source("global.R", local = T) 
  source("utils.R" , local = T)
  
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
  updateAceEditor(session, "fieldCodeResum", value = cod.resum())
  updateAceEditor(session, "fieldModelCor" , value = modelo.cor())
  updateAceEditor(session, "fieldFuncNum"  , extract.code("distribucion.numerico"))
  updateAceEditor(session, "fieldFuncCat"  , extract.code("distribucion.categorico"))

  # REACTIVE VALUES -------------------------------------------------------------------------------------------------------

  updatePlot <- reactiveValues(calc.normal = default.calc.normal(), 
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

  # PAGE TO LOAD AND TRANSFORM DATA ---------------------------------------------------------------------------------------

  # Executes the data upload code
  upload_data <- function(codigo.carga = "") {
    tryCatch({
      isolate(exe(codigo.carga))
      if(ncol(datos) <= 1) {
        showNotification(translate("errorCData"), duration = 10, type = "error")
        return(NULL)
      }
      new.report()
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
        codigo.na <- paste0(code.NA(deleteNA = input$deleteNA), "\n", "datos <<- datos.originales")
        isolate(exe(codigo.na))
        insert.report("na.delete",paste0("\n# Imputación de Datos\n```{r}\n",codigo.na,"\nhead(datos)\nstr(datos)\n```"))
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

  # Transforma los datos - OJO
  transformar.datos <- function() {
    var.noactivas <- c()
    code.res <- "datos <<- datos.originales \n"
    for (var in colnames(datos.originales)) {
      if (input[[paste0("box", var, contador)]]) {
        if (input[[paste0("sel", var, contador)]] == "categorico" & class(datos.originales[, var]) %in% c("numeric", "integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if (input[[paste0("sel", var, contador)]] == "numerico" & !(class(datos.originales[, var]) %in% c("numeric", "integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico"), "\n")
        }
        if (input[[paste0("sel", var, contador)]] == "disyuntivo") {
          code.res <- paste0(code.res, code.trans(var, "disyuntivo"), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }

    isolate(exe(code.res))
    if (length(var.noactivas) > 0) {
      isolate(exe(code.desactivar(var.noactivas)))
    }

    code.res <- paste0(code.res, "\n", code.desactivar(var.noactivas))
    new.secction.report()
    insert.report("transformar.datos",paste0("# Transformando Datos\n```{r}\n",code.res,"\nstr(datos)\n```"))
    return(code.res)
  }

  # Update the different selectors
  aqualize_selecctors <- function() {
    updateSelectizeInput(session, "sel.normal", choices = colnames.empty(var.numerical(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames.empty(var.numerical(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames.empty(var.numerical(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames.empty(var.categorical(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames.empty(datos))
    updateSelectInput(session, "sel.predic.var", choices = rev(colnames.empty(var.numerical(datos))))
  }

  # Executes the code of correlations
  run_cor_model <- function() {
    tryCatch({
      isolate(exe(text = modelo.cor()))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
  }

  # Clears model data
  delete_models <- function(flag.datos = TRUE) {
    if (flag.datos) {
      datos.prueba <<- NULL
      datos.aprendizaje <<- NULL
      variable.predecir <<- NULL
      real.val <<- NULL
    }

    IndicesM <<- list()

    rm(list = nombres.modelos, envir = .GlobalEnv)
    nombres.modelos <<- c()

    updateCheckboxGroupButtons(session, inputId = "select.models",
                               choices = c(" ---- " = "NoDisponible"),
                               size = "sm", status = "primary",
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                no = icon("remove", lib = "glyphicon")))

    updateSelectInput(session,"kernel.knn", selected = "optimal")
  }

  # When the load data button is pressed
  observeEvent(input$loadButton, {
    codigo.carga <- code.carga(nombre.filas = input$rowname, ruta = input$file1$datapath,
                               separador = input$sep, sep.decimal = input$dec, encabezado = input$header)

    upload_data(codigo.carga)

    codigo.na <- clean_data()

    updateAceEditor(session, "fieldCodeData", value = paste0(codigo.carga, "\n", codigo.na))

    aqualize_selecctors()

    run_cor_model()

    delete_models()

    close_menu("parte1"   , is.null(datos))
    close_menu("parte2"   , is.null(datos.aprendizaje))
    close_menu("comparar" , is.null(datos.aprendizaje))
    close_menu("poderPred", is.null(datos.aprendizaje))

    update_table()
  }, priority = 4)

  # When the button to transform data is pressed
  observeEvent(input$transButton, {
    code.res <- transformar.datos()

    updateAceEditor(session, "fieldCodeTrans", value = code.res)

    aqualize_selecctors()

    run_cor_model()

    delete_models()

    close_menu("parte1"   , is.null(datos))
    close_menu("parte2"   , is.null(datos.aprendizaje))
    close_menu("comparar" , is.null(datos.aprendizaje))
    close_menu("poderPred", is.null(datos.aprendizaje))

    update_table()
  }, priority = 4)

  # Crea los select box del panel de trasnformar datos - OJO
  update.trans <- eventReactive(input$loadButton, {
    contador <<- contador + 1
    if(!is.null(datos) && ncol(datos) > 0){
      res <- data.frame(Variables = colnames(datos), Tipo = c(1:ncol(datos)), Activa = c(1:ncol(datos)))
      res$Tipo <- sapply(colnames(datos), function(i)
        paste0('<select id="sel', i, contador, '"> <option value="categorico">',translate("categorico"),'</option>',
               '<option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric", "integer"),
                                                   ' selected="selected"', ""),'>', translate("numerico"),
               '</option> <option value="disyuntivo">',translate("disyuntivo"),'</option> </select>'))
      res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id="box', i, contador, '" checked/>'))
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
                                    DT::datatable(update.trans(),
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
      codigo <- particion.code("datos", input$segmentacionDatosA,
                               input$sel.predic.var,
                               input$semilla,
                               input$permitir.semilla)

      semilla       <<- input$permitir.semilla
      knn.stop.excu <<- FALSE
      rf.stop.excu  <<- FALSE

      segmentar.datos(codigo)

      new.secction.report()
      insert.report("segmentar.datos",paste0("\n# Datos de Aprendizaje\n```{r}\n",codigo,
                                             "\nhead(datos.aprendizaje)\n```\n\n# Datos de Prueba\n```{r}\nhead(datos.prueba)\n```\n"))

      delete_models(FALSE)

      # change model codes
      deafult_codigo_rl()
      deafult_codigo_rlr()
      default_codigo_knn(k.def = TRUE)
      default_codigo_svm()
      default_codigo_dt()
      deafult_codigo_rf(rf.def = TRUE)
      deault_codigo_boosting()
      default_codigo_nn()
      
    } else {
      showNotification(translate("tieneSVP"), duration = 15, type = "error")
    }

    close_menu("parte2",    is.null(datos.aprendizaje))
    close_menu("comparar",  is.null(datos.aprendizaje))
    close_menu("poderPred", is.null(datos.aprendizaje))
    
    update_table(c("datos.aprendizaje", "datos.prueba"))
    
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

  # SUMMARY PAGE ----------------------------------------------------------------------------------------------------------

  # Change the table with the summary on the summary page
  output$resumen.completo <- DT::renderDataTable({ insert.report("resumen" ,c(paste0("\n## Resumen Numérico \n```{r} \nsummary(datos) \n```")))
                                                   data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
                                                 }, options = list(dom = "ft", scrollX = TRUE), rownames = F)

  # Change summary tables by variable
  output$resumen <- renderUI({
    if (input$sel.resumen %in% colnames(var.numerical(datos))){
      resumen.numerico(datos, input$sel.resumen)
    }else{
      resumen.categorico(datos, input$sel.resumen)
    }
  })

  # NORMALITY TEST PAGE ---------------------------------------------------------------------------------------------------

  # Show the graph of the normality test page
  observeEvent(c(input$loadButton, input$transButton), {
   output$plot.normal <- renderPlot({
      tryCatch({
        cod.normal <<- updatePlot$normal
        res <- isolate(exe(cod.normal))
        updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
        insert.report(paste0("normalidad.", input$sel.normal),paste0("## Test de Normalidad \n```{r}\n", cod.normal, "\n```"))
        return(res)
      }, error = function(e){
        if(ncol(var.numerical(datos)) <= 0){
          error.variables( T)
        } else {
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Change the code in the code field
  observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })

  # Executes the code when parameters change
  observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- default.normal(data = "datos", vars = input$sel.normal, color = input$col.normal, translate("curvanormal"))
  })

  # Show the comparative table of the normality test page
  observeEvent(c(input$loadButton, input$transButton), {
    output$calculo.normal <- DT::renderDT({
      tryCatch({
        codigo <- updatePlot$calc.normal
        res    <- isolate(exe(codigo))
        updateAceEditor(session, "fieldCalcNormal", value = codigo)
        fisher    <- translate("fisher")
        asimetria <- translate("asimetria")
        sketch = htmltools::withTags(table(tags$thead(tags$tr(tags$th(), tags$th(fisher), tags$th(asimetria)))))
        DT::datatable(res, selection = 'none', container = sketch, options = list(dom = 'frtip', scrollY = "40vh"))
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  # Run the comparison table
  observeEvent(input$run.calc.normal, {
    updatePlot$calc.normal <- input$fieldCalcNormal
  })

  # DISPERSION PAGE -------------------------------------------------------------------------------------------------------

  # Show the scatter plot
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.disp <- renderPlot({
      tryCatch({
        cod.disp <<- updatePlot$disp
        updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
        if(!is.null(cod.disp) && cod.disp != "") {
          insert.report(paste0("dispersion.", paste(input$select.var, collapse = ".")),
                        paste0("## Dispersión \n```{r}\n", cod.disp, "\n```"))
        }
        return(isolate(exe(cod.disp)))
      }, error = function(e) {
        if(ncol(var.numerical(datos)) <= 1){
          error.variables( T)
        } else {
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Show the zoom graph
  output$plot.disp.zoom <- renderPlot({
    tryCatch({
      cod.disp <<- updatePlot$disp
      res <- isolate(exe(cod.disp))
      res <- res + coord_cartesian(xlim = disp.ranges$x, ylim = disp.ranges$y, expand = FALSE)
      return(res)
    }, error = function(e) {
      return(NULL)
    })
  })

  # Show the table with the dispersion values
  output$mostrar.disp.zoom <- DT::renderDataTable({
    tryCatch({
      return(brushedPoints(datos[, input$select.var], input$zoom.disp))
    }, error = function(e) {
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh", pageLength = nrow(datos)))

  # When a zoom area is selected
  observe({
    brush <- input$zoom.disp
    if (!is.null(brush)) {
      disp.ranges$x <- c(brush$xmin, brush$xmax)
      disp.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      disp.ranges$x <- NULL
      disp.ranges$y <- NULL
    }
  })

  # Change the graphic code
  observeEvent(input$run.disp, {
    updatePlot$disp <- input$fieldCodeDisp
  })

  # Executes the code when parameters change
  observeEvent(c(input$select.var, input$col.disp), {
    if (length(input$select.var) < 2) {
      updatePlot$disp <- ""
    } else {
      updatePlot$disp <<- default.disp(data = "datos", vars = input$select.var, color = input$col.disp)
    }
  })

  # DISTRIBUTION PAGE -----------------------------------------------------------------------------------------------------

  # Show the graph of numerical distribution
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.num <- renderPlot({
      tryCatch({
        cod.dya.num <<- updatePlot$dya.num
        res <- isolate(exe(cod.dya.num))
        updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
        insert.report(paste0("dya.num.", input$sel.distribucion.num),
                      paste0("## Distribución y atipicidad \n```{r}\n", cod.dya.num,"\n```"))
        
        return(res)
      }, error = function(e) {
        if (ncol(var.numerical(datos)) == 0){
          error.variables( T)
        }else{
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Execute the code of the numerical chart
  observeEvent(input$run.dya.num, {
    updatePlot$dya.num <- input$fieldCodeNum
  })

  # Executes the code when parameters change
  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <<- def.code.num(data = "datos", color = paste0("'", input$col.dist, "'"),
                                        variable = paste0("'", input$sel.distribucion.num, "'"))
  })
  
  # Creates the atypical table
  observeEvent(c(input$distribucion_numerica), {
    output$mostrarAtipicos <- DT::renderDataTable({
      atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
      datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
      datos <- datos[order(datos[, input$sel.distribucion.num]), , drop = F]
      datatable(datos, options = list(dom = 't', scrollX = TRUE, scrollY = "28vh",pageLength = nrow(datos))) %>%
        formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
    })
  })
  
  # Show the graph of categorical distribution
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.cat <- renderPlot({
      tryCatch({
        cod.dya.cat <<- updatePlot$dya.cat
        res <- isolate(exe(cod.dya.cat))
        updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
        insert.report(paste0("dya.cat.", input$sel.distribucion.cat),
                      paste0("## Distribución \n```{r}\n", cod.dya.cat, "\n```"))
        return(res)
      }, error = function(e) {
        if (ncol(var.categorical(datos)) == 0){
          error.variables( T)
        }else{
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Change the code of the categorical graphic
  observeEvent(input$run.dya.cat, {
    updatePlot$dya.cat <- input$fieldCodeCat
  })

  # Executes the code when parameters change
  observeEvent(input$sel.distribucion.cat, {
    updatePlot$dya.cat <<- def.code.cat(variable = input$sel.distribucion.cat)
  })

  # CORRELATION PAGE ------------------------------------------------------------------------------------------------------

  # Show the correlation graph
  observeEvent(c(input$loadButton, input$transButton, input$fieldModelCor), {
    output$plot.cor <- renderPlot({
      tryCatch({
        cod.cor <<- updatePlot$cor
        res <- isolate(exe(cod.cor))
        updateAceEditor(session, "fieldCodeCor", value = cod.cor)
        insert.report("correlacion", paste0("## Correlación \n```{r}\n", cod.cor, "\n```"))
        return(res)
      }, error = function(e) {
        if (ncol(var.numerical(datos)) == 0){
          error.variables( T)
        }else{
          showNotification(paste0("ERROR EN Correlacion: ", e),
                           duration = 10,
                           type = "error")
          return(NULL)
        }
      })
    })
  })

  # Change the graphic code
  observeEvent(input$run.code.cor, {
    updatePlot$cor <- input$fieldCodeCor
  })

  # Executes the code when parameters change
  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlaciones(metodo = input$cor.metodo, tipo = input$cor.tipo)
  })

  # PREDICTIVE POWER PAGE -------------------------------------------------------------------------------------------------

  # Show the graph of numerical predictive power
  observeEvent(input$segmentButton,{
    output$plot.pairs.poder <- renderPlot({
      tryCatch({
        cod.poder.num <<- updatePlot$poder.num
        updateAceEditor(session, "fieldCodePoderNum", value = cod.poder.num)
        if (ncol(var.numerical(datos)) >= 2) {
          if(ncol(var.numerical(datos)) <= 25){
            res <- isolate(exe(cod.poder.num))
            insert.report("poder.num",paste0("## Poder Predictivo Variables Numéricas \n```{r}\n", cod.poder.num, "\n```"))
            return(res)
          }else{
            showNotification(translate("bigPlot"), duration = 10, type = "message")
            return(NULL)
          }
        }else{
          error.variables( T)
        }
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
  })

  # Execute the graphic code
  observeEvent(input$run.code.poder.num, {
    if(input$fieldCodePoderNum != "") {
      updatePlot$poder.num <- input$fieldCodePoderNum
    } else {
      updatePlot$poder.num <- pairs.poder
    }
  })

  # Change the graphic code
  observeEvent(input$segmentButton,{
    updatePlot$poder.num <- pairs.poder
  }, priority = 3)
  
  # RL PAGE ---------------------------------------------------------------------------------------------------------------
  
  # When the rl model is generated
  observeEvent(input$runRl, {
    if (validate_data()) { # Si se tiene los datos entonces :
      rl_full()
    }
  })
  
  # Upgrade code fields to default version
  deafult_codigo_rl <- function(){
    # Se acualiza el codigo del modelo
    codigo <- rl.modelo(variable.pr = variable.predecir)
    
    updateAceEditor(session, "fieldCodeRl", value = codigo)
    cod.rl.modelo <<- codigo
    
    # Se genera el codigo de la prediccion
    codigo <- rl.prediccion(variable.predecir)
    updateAceEditor(session, "fieldCodeRlPred", value = codigo)
    cod.rl.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- rl.disp()
    updateAceEditor(session, "fieldCodeRlDisp", value = codigo)
    
    # Se genera el codigo de la indices
    codigo <- extract.code("general.indices")
    updateAceEditor(session, "fieldCodeRlIG", value = codigo)
    cod.rl.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_rl <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rl <<- NULL
        output$txtRl <- renderPrint(invisible(""))
        remove.report.elem("modelo.rl")
        remove.report.elem("disp.rl")
      }, {
        prediccion.rl <<- NULL
        remove.report.elem("pred.rl")
        output$rlPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rl <<- rep(0, 10)
        remove.report.elem("ind.rl")
      })
    }
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rl <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.rl.disp <- renderPlot(exe(input$fieldCodeRlDisp))
      insert.report("disp.rl",
                    paste0("## Dispersión del Modelo RL\n```{r}\n", input$fieldCodeRlDisp,"\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(2)
      showNotification(paste0("Error (RL-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  rl_full <- function(){
    execute_rl()
    execute_rl_pred()
    execute_rl_ind()
  }
  
  # Generates the model
  execute_rl <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rl.modelo))
      output$txtRl <- renderPrint(print(summary(modelo.rl)))
      
      insert.report("modelo.rl",paste0("## Generación del Modelo Bosques Aleatorios\n```{r}\n",
                                       cod.rl.modelo, "\nmodelo.rl\n```"))
      
      nombres.modelos <<- c(nombres.modelos, "modelo.rl")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(1)
      showNotification(paste0("Error (RL-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_rl_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rl.pred))
      
      output$rlPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.rl), server = FALSE)
      
      insert.report("pred.rl",
                    paste0("## Predicción del Modelo Bosques Aleatorios\n```{r}\n", cod.rl.pred,
                           "\nhead(tb_predic(real.val, prediccion.rl)x$data)\n```"))
      
      plot_disp_rl()
      
      nombres.modelos <<- c(nombres.modelos, "prediccion.rl")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(2)
      showNotification(paste0("Error (RL-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_rl_ind <- function() {
    if(exists("prediccion.rl")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rl.ind))
        
        indices.rl <<- general.indices(datos.prueba[,variable.predecir], prediccion.rl)
        
        insert.report("ind.rl",paste0("## Índices Generales\n```{r}\n",
                                      cod.rl.ind, "\ngeneral.indices(datos.prueba[,'",variable.predecir,"'], prediccion.rl)\n",
                                      "indices.rl<- general.indices(datos.prueba[,'",variable.predecir,"'], prediccion.rl)\n",
                                      "IndicesM[['rll']] <<- indices.rl\n```"))
        

        df <- as.data.frame(indices.rl)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrl <- render.index.table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrl2 <- render.index.table(df2)
        
        nombres.modelos <<- c(nombres.modelos, "indices.rl")
        IndicesM[["rll"]] <<- indices.rl
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_rl(3)
        showNotification(paste0("Error (RL-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # RLR PAGE --------------------------------------------------------------------------------------------------------------
  
  # When the rlr model is generated
  observeEvent(input$runRlr, {
    if (validate_data()) { # If you have the data then :
      rlr_full()
    }
  })
  
  # When the user changes the parameters
  observeEvent(c(input$alpha.rlr, input$switch.scale.rlr, input$landa, input$permitir.landa), {
    if (validate_data(print = FALSE)) {
      deafult_codigo_rlr()
    }
  })
  
  # When user press enable or disable the lambda
  observeEvent(input$permitir.landa, {
    if (input$permitir.landa) {
      shinyjs::enable("landa")
    } else {
      shinyjs::disable("landa")
    }
  })

  # Upgrade code fields to default version
  deafult_codigo_rlr <- function(){
    landa <- NULL
    
    if (input$permitir.landa) {
      if (input$landa >= 0) {
        landa <- input$landa
      }
    }
    
    # The model code is updated
    codigo <- rlr.modelo(variable.pr = variable.predecir,
                         input$alpha.rlr,
                         input$switch.scale.rlr)

    updateAceEditor(session, "fieldCodeRlr", value = codigo)
    cod.rlr.modelo <<- codigo

    # The code of the possible landa is generated
    codigo <- select.landa(variable.predecir,
                           input$alpha.rlr,
                           input$switch.scale.rlr)
    updateAceEditor(session, "fieldCodeRlrPosibLanda", value = codigo)
    cod.select.landa <<- codigo

    # The code that prints the coefficients is generated
    codigo <- coeff.landas(landa)
    updateAceEditor(session, "fieldCodeRlrCoeff", value = codigo)
    
    # The code of the coefficients is generated with the best lambda
    codigo <- plot.coeff.landa(landa)
    updateAceEditor(session, "fieldCodeRlrLanda", value = codigo)
    
    # The prediction code is generated
    codigo <- rlr.prediccion(landa)
    updateAceEditor(session, "fieldCodeRlrPred", value = codigo)
    cod.rlr.pred <<- codigo

    # The dispersion code is generated
    codigo <- rlr.disp()
    updateAceEditor(session, "fieldCodeRlrDisp", value = codigo)

    # The index code is generated
    codigo <- extract.code("general.indices")
    updateAceEditor(session, "fieldCodeRlrIG", value = codigo)
    cod.rlr.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_rlr <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rlr <<- NULL
        output$txtRlr <- renderPrint(invisible(""))
        remove.report.elem(paste0("modelo.rlr.",rlr.type()))
        remove.report.elem(paste0("disp.rlr.",rlr.type()))
        remove.report.elem(paste0("landa.rlr.",rlr.type()))
      }, {
        prediccion.rlr <<- NULL
        remove.report.elem(paste0("pred.rlr.",rlr.type()))
        output$rlrPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rlr <<- rep(0, 10)
        remove.report.elem(paste0("ind.rlr",rlr.type()))
      })
    }
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rlr <- function(){
    tryCatch({ # Se corren los codigo
      codigo <- input$fieldCodeRlrDisp
      output$plot.rlr.disp <- renderPlot(isolate(exe(codigo)))
      insert.report(paste0("disp.rlr.",rlr.type()),
                    paste0("## Dispersión del Modelo R/L\n```{r}\n", codigo ,"\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }

  # Show the graph of the possible lambda
  plot_posib_landa_rlr <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.select.landa))
      output$plot.rlr.posiblanda <- renderPlot(exe("plot(cv.glm.",rlr.type(),")"))
      insert.report(paste0("posib.landa.rlr.",rlr.type()),
                    paste0("\nPosible lambda\n```{r}\n", cod.select.landa,"\n",
                           "plot(cv.glm.",rlr.type(),")\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }

  # Displays coefficients as text
  print_coeff <- function(){
    tryCatch({ # Se corren los codigo
      codigo <- input$fieldCodeRlrCoeff
      output$txtRlrCoeff <- renderPrint(print(isolate(exe(codigo))))
      insert.report(paste0("coeff.landa.rlr.",rlr.type()),
                    paste0("\n##Coeficientes\n```{r}\n", codigo ,"\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Show the graph of the coefficients
  plot_coeff <- function(){
    tryCatch({ # Se corren los codigo
      codigo <- input$fieldCodeRlrLanda
      output$plot.rlr.landa <- renderPlot(isolate(exe(codigo)))
      insert.report(paste0("gcoeff.landa.rlr.",rlr.type()),
                    paste0("\n##Coeficientes y lamdas\n```{r}\n", codigo,"\n```\n"))
    },
    error = function(e){ # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  rlr_full <- function(){
    execute_rlr()
    execute_rlr_pred()
    execute_rlr_ind()
  }
   
  # Generates the model
  execute_rlr <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rlr.modelo))
      isolate(tipo <- rlr.type())
      output$txtRlr <- renderPrint(print(exe("modelo.rlr.",tipo)))

      insert.report(paste0("modelo.rlr.",tipo),paste0("## Generación del Modelo R/L\n```{r}\n",
                                       cod.rlr.modelo, "\nmodelo.rlr.",tipo,"\n```"))

      plot_posib_landa_rlr()
      print_coeff()
      plot_coeff()
      
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.rlr.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(1)
      showNotification(paste0("Error (R/L-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_rlr_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rlr.pred))
      isolate(tipo <- rlr.type())
      output$rlrPrediTable <- DT::renderDataTable(tb_predic(real.val, exe("prediccion.rlr.",tipo)), server = FALSE)

      insert.report(paste0("pred.rlr.",tipo),
                    paste0("## Predicción del R/L\n```{r}\n", cod.rlr.pred,
                           "\nhead(tb_predic(real.val, prediccion.rlr.",tipo,")x$data)\n```"))

      plot_disp_rlr()
      nombres.modelos <<- c(nombres.modelos, "prediccion.rlr")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_rlr_ind <- function() {
    if(exists(paste0("prediccion.rlr.",rlr.type()))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rlr.ind))

        indices.rlr <- general.indices(datos.prueba[,variable.predecir], exe("prediccion.rlr.",rlr.type()))
        
        insert.report(paste0("ind.rlr.",rlr.type()),paste0("## Índices Generales\n```{r}\n",
                                      cod.rlr.ind, "\ngeneral.indices(datos.prueba[,'",variable.predecir,"'], prediccion.rlr.",rlr.type(),")\n",
                                      "indices.rlr <- general.indices(datos.prueba[,'",variable.predecir,"'], prediccion.rlr.",rlr.type(),")\n",
                                      "IndicesM[['rlr-",rlr.type(),"']] <<- indices.rlr\n```"))

        
        df <- as.data.frame(indices.rlr)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrlr <- render.index.table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrlr2 <- render.index.table(df2)

        IndicesM[[paste0("rlr-",rlr.type())]] <<- indices.rlr
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_rlr(3)
        showNotification(paste0("Error (R/L-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # KNN PAGE --------------------------------------------------------------------------------------------------------------

  # When the knn model is generated
  observeEvent(input$runKnn, {
    if (validate_data()) { # Si se tiene los datos entonces :
      knn_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$switch.scale.knn, input$kmax.knn, input$kernel.knn), {
    if (validate_data(print = FALSE) & knn.stop.excu) {
      default_codigo_knn()
    }else{
      knn.stop.excu <<- TRUE
    }
  })

  # Upgrade code fields to default version
  default_codigo_knn <- function(k.def = FALSE) {
    if(!is.null(datos.aprendizaje) & k.def){
      k.value <- ifelse(k.def, round(sqrt(nrow(datos.aprendizaje))), input$kmax.knn)
      updateNumericInput(session,"kmax.knn",value = k.value)
    }else{
      k.value <- input$kmax.knn
    }

    # Se acualiza el codigo del modelo
    codigo <- kkn.modelo(
      variable.pr = variable.predecir,
      scale = input$switch.scale.knn,
      kmax = k.value,
      kernel = input$kernel.knn)
    
    updateAceEditor(session, "fieldCodeKnn", value = codigo)
    cod.knn.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- kkn.prediccion(kernel = input$kernel.knn)
    updateAceEditor(session, "fieldCodeKnnPred", value = codigo)
    cod.knn.pred <<- codigo

    # Se genera el codigo de la dispersion
    codigo <- knn.disp(input$kernel.knn)
    updateAceEditor(session, "fieldCodeKnnDisp", value = codigo)

    # Se genera el codigo de la indices
    codigo <- extract.code("general.indices")
    updateAceEditor(session, "fieldCodeKnnIG", value = codigo)
    cod.knn.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_knn <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        exe("modelo.knn.",input$kernel.knn," <<- NULL")
        output$txtknn <- renderPrint(invisible(""))
        remove.report.elem(paste0("modelo.knn.",input$kernel.knn))
      }, {
        exe("prediccion.knn.",input$kernel.knn," <<- NULL")
        remove.report.elem(paste0("pred.knn.",input$kernel.knn))
        output$knnPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("indices.knn.",input$kernel.knn," <<- NULL")
        remove.report.elem(paste0("ind.knn.",input$kernel.knn))
      })
    }
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_knn <- function(){
    tryCatch({ # Se corren los codigo
      codigo <- input$fieldCodeKnnDisp
      isolate(kernel <- input$kernel.knn)
      output$plot.knn.disp <- renderPlot(exe(codigo))
      insert.report(paste0("disp.knn.",kernel),
                    paste0("## Dispersión del Modelo KNN - ",kernel,"\n```{r}\n", codigo,"\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_knn(2)
      showNotification(paste0("Error (KNN-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  knn_full <- function() {
    execute_knn()
    execute_knn_pred()
    execute_knn_ind()
  }

  # Generates the model
  execute_knn <- function() {
    tryCatch({
      exe(cod.knn.modelo)
      isolate(kernel <- input$kernel.knn)
      updateAceEditor(session, "fieldCodeKnn", value = cod.knn.modelo)
      output$txtknn <- renderPrint(exe("modelo.knn.",kernel))
      insert.report(paste0("modelo.knn.",kernel),
                    paste0("## Generación del modelo KNN - ",kernel,"\n```{r}\n",cod.knn.modelo, "\nmodelo.knn.",kernel,"\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("modelo.knn.",kernel))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_knn(1)
      showNotification(paste0("Error (KNN-01) : ", e), duration = 15, type = "error")
    }
    )
  }

  # Generate the prediction
  execute_knn_pred <- function() {
    tryCatch({ # Se corren los codigo
      exe(cod.knn.pred)
      isolate(kernel <- input$kernel.knn)
      # Cambia la tabla con la prediccion de knn
      output$knnPrediTable <- DT::renderDataTable(tb_predic(real.val, exe("prediccion.knn.",kernel)),server = FALSE)
      insert.report(paste0("pred.knn.",kernel),
                    paste0("## Predicción del Modelo KNN - ",kernel,"\n```{r}\n", cod.knn.pred,
                           "\nhead(tb_predic(real.val, prediccion.knn.",kernel,")x$data)\n```"))

      plot_disp_knn()
      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.knn.",kernel))
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_knn(2)
      showNotification(paste0("Error (KNN-02) : ", e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_knn_ind <- function(){
    if(exists(paste0("prediccion.knn.",input$kernel.knn))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.knn.ind))
        isolate(kernel <- input$kernel.knn)
        
        indices.knn <- general.indices(datos.prueba[,variable.predecir], exe("prediccion.knn.",kernel))
        eval(parse(text = paste0("indices.knn.",kernel, "<<- indices.knn")))

        insert.report(paste0("ind.knn.",kernel),
                      paste0("## Índices Generales del Modelo KNN - ",kernel,"\n```{r}\n",
                             cod.knn.ind, "\ngeneral.indices(datos.prueba[,'",variable.predecir,"'] ,prediccion.knn.",kernel,")\n",
                             "indices.knn <- general.indices(datos.prueba[,'",variable.predecir,"'], prediccion.knn.",kernel,")\n",
                             "IndicesM[['knnl-",kernel,"']] <<- indices.knn\n```"))

        
        df <- as.data.frame(indices.knn)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfknn <- render.index.table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfknn2 <- render.index.table(df2)

        nombres.modelos <<- c(nombres.modelos, paste0("indices.knn.",kernel))
        IndicesM[[paste0("knnl-",kernel)]] <<- indices.knn
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_knn(3)
        showNotification(paste0("Error (KNN-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # SVM PAGE --------------------------------------------------------------------------------------------------------------

  # When the knn model is generated
  observeEvent(input$runSvm, {
    if (validate_data()) { # Si se tiene los datos entonces :
      svm_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$switch.scale.svm, input$kernel.svm), {
    if (validate_data(print = FALSE)){
      default_codigo_svm()
    }
  })

  # Upgrade code fields to default version
  default_codigo_svm <- function() {
    # Se acualiza el codigo del modelo
    codigo <- svm.modelo(variable.pr = variable.predecir,
                         scale = input$switch.scale.svm,
                         kernel = input$kernel.svm)

    updateAceEditor(session, "fieldCodeSvm", value = codigo)
    cod.svm.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- svm.prediccion(input$kernel.svm)
    updateAceEditor(session, "fieldCodeSvmPred", value = codigo)
    cod.svm.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- svm.disp(input$kernel.svm)
    updateAceEditor(session, "fieldCodeSvmDisp", value = codigo)

    # Se genera el codigo de la indices
    codigo <- extract.code("general.indices")
    updateAceEditor(session, "fieldCodeSvmIG", value = codigo)
    cod.svm.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_svm <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        exe("modelo.svm.",input$kernel.svm,"<<- NULL")
        output$txtSvm <- renderPrint(invisible(""))
        remove.report.elem(paste0("modelo.svm.",input$kernel.svm))
        # remove.report.elem(grepl(paste0("svm.plot.",input$kernel.svm), names.report()))
      }, {
        exe("prediccion.svm.",input$kernel.svm,"<<- NULL")
        remove.report.elem(paste0("pred.svm.",input$kernel.svm))
        output$svmPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("indices.svm.",input$kernel.svm,"<<- NULL")
        remove.report.elem(paste0("ind.svm.",input$kernel.svm))
      })
    }
  }

  # Execute model, prediction and indices
  svm_full <- function() {
    execute_svm()
    execute_svm_pred()
    execute_svm_ind()
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_svm <- function(){
    tryCatch({ # Se corren los codigo
      isolate(kernel <- input$kernel.svm)
      codigo <- input$fieldCodeSvmDisp
      output$plot.svm.disp <- renderPlot(exe(codigo))
      insert.report(paste0("disp.svm.",kernel),
                    paste0("## Dispersión del Modelo SVM - ",kernel,"\n```{r}\n", codigo,"\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(2)
      showNotification(paste0("Error (SVM-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the model
  execute_svm <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.svm.modelo))
      isolate(kernel <- input$kernel.svm)
      output$txtSvm <- renderPrint(exe("print(modelo.svm.",kernel,")"))
      updateAceEditor(session, "fieldCodeSvm", value = cod.svm.modelo)

      insert.report(paste0("modelo.svm.",kernel),
                    paste0("## Generación del modelo SVM - ",kernel,"\n```{r}\n",
                           cod.svm.modelo, "\nmodelo.svm.",kernel,"\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("modelo.svm.", kernel))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(1)
      showNotification(paste0("Error (SVM-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_svm_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.svm.pred))
      isolate(kernel <- input$kernel.svm)
      
      # Cambia la tabla con la prediccion de knn
      output$svmPrediTable <- DT::renderDataTable(exe("tb_predic(real.val, prediccion.svm.",kernel,")"),server = FALSE)
      insert.report(paste0("pred.svm.",input$kernel.svm),
                    paste0("## Predicción del Modelo SVM - ",kernel,"\n```{r}\n", cod.svm.pred,
                           "\nhead(tb_predic(real.val, prediccion.svm.",kernel,")x$data)\n```"))

      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.svm.",kernel))

      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(2)
      showNotification(paste0("Error (SVM-02) : ",e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_svm_ind <- function(){
    if(exists(paste0("prediccion.svm.",input$kernel.svm))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.svm.ind))
        isolate(kernel <- input$kernel.svm)
        
        indices.svm <- general.indices(datos.prueba[,variable.predecir], exe("prediccion.svm.",kernel))
        eval(parse(text =paste0("indices.svm.",kernel, "<<- indices.svm")))

        insert.report(paste0("ind.svm.",kernel),
                      paste0("## Índices Generales del modelo SVM  - ",kernel," \n```{r}\n",
                             cod.svm.ind, "\ngeneral.indices(datos.prueba[,'",variable.predecir,"'], prediccion.svm.",kernel,")\n",
                             "indices.svm <- general.indices(datos.prueba[,'",variable.predecir,"'], prediccion.svm.",kernel,")\n",
                             "IndicesM[['svml-",kernel,"']] <<- indices.svm\n```"))

        
        df <- as.data.frame(indices.svm)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfsvm <- render.index.table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfsvm2 <- render.index.table(df2)
        
        plot_disp_svm()
        nombres.modelos <<- c(nombres.modelos, paste0("indices.svm.",kernel))
        IndicesM[[paste0("svml-",kernel)]] <<- exe("indices.svm.",kernel)
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_svm(3)
        showNotification(paste0("Error (SVM-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # DT PAGE ---------------------------------------------------------------------------------------------------------------

  #  When the dt model is generated
  observeEvent(input$runDt, {
    if (validate_data()) { # Si se tiene los datos entonces :
      dt_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$minsplit.dt, input$maxdepth.dt), {
    if (validate_data(print = FALSE)){
      default_codigo_dt()
    }
  })

  # Upgrade code fields to default version
  default_codigo_dt <- function() {

    # Se acualiza el codigo del modelo
    codigo <- dt.modelo(variable.pr = variable.predecir,
                        minsplit = input$minsplit.dt,
                        maxdepth = input$maxdepth.dt)

    updateAceEditor(session, "fieldCodeDt", value = codigo)
    cod.dt.modelo <<- codigo

    # Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeDtPlot", value = dt.plot())

    # Se genera el codigo de la prediccion
    codigo <- dt.prediccion()
    updateAceEditor(session, "fieldCodeDtPred", value = codigo)
    cod.dt.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- dt.disp()
    updateAceEditor(session, "fieldCodeDtDisp", value = codigo)

    # Se genera el codigo de la indices
    codigo <- extract.code("general.indices")
    updateAceEditor(session, "fieldCodeDtIG", value = codigo)
    cod.dt.ind <<- codigo
  }

  # Shows the graph of the tree
  plot_tree <- function(){
    tryCatch({
      output$plot.dt <- renderPlot(isolate(exe(input$fieldCodeDtPlot)))
      cod <- ifelse(input$fieldCodeDtPlot == "", dt.plot(), input$fieldCodeDtPlot)
      insert.report("modelo.dt.graf", paste0("\n```{r}\n", cod, "\n```"))
    },
    error = function(e){
      output$plot.dt <- renderPlot(NULL)
      insert.report("modelo.dt.graf",NULL)
    })
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_dt <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.dt.disp <- renderPlot(exe(input$fieldCodeDtDisp))
      insert.report("disp.dt",
                    paste0("## Dispersión del Modelo Árboles de Decisión\n```{r}\n", input$fieldCodeDtDisp,"\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_dt(2)
      showNotification(paste0("Error (DT-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Shows the rules of the tree
  show_dt_rules <- function(){
    output$rulesDt <- renderPrint(rattle::asRules(modelo.dt))
    updateAceEditor(session, "fieldCodeDtRule", paste0("asRules(modelo.dt)"))
    insert.report("modelo.dt.rules",
                  paste0("\n```{r}\nrattle::asRules(modelo.dt)\n```"))
  }

  # Cleans the data according to the process where the error is generated
  clean_dt <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        modelo.dt <<- NULL
        output$txtDt <- renderPrint(invisible(""))
        output$plot.dt <- renderPlot(NULL)
        remove.report.elem("modelo.dt")
        remove.report.elem("modelo.dt.graf")
        remove.report.elem("disp.dt")
      }, {
        prediccion.dt <<- NULL
        remove.report.elem("pred.dt")
        output$dtPrediTable <- DT::renderDataTable(NULL)
      }, {
        indices.dt <<- rep(0, 10)
        remove.report.elem("ind.dt")
      })
    }
  }

  # Execute model, prediction and indices
  dt_full <- function() {
    execute_dt()
    execute_dt_pred()
    execute_dt_ind()
  }

  # Generates the model
  execute_dt <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.dt.modelo))
      output$txtDt <- renderPrint(print(modelo.dt))
      insert.report("modelo.dt",
                    paste0("## Generación del modelo Árboles de Decisión\n```{r}\n", cod.dt.modelo,
                           "\nmodelo.dt\n```"))
      plot_tree()
      show_dt_rules()
      nombres.modelos <<- c(nombres.modelos, "modelo.dt")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_dt(1)
      showNotification(paste0("Error (DT-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_dt_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.dt.pred))
      # Cambia la tabla con la prediccion de dt
      output$dtPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.dt),server = FALSE)

      insert.report("pred.dt",
                    paste0("## Predicción del Modelo Árboles de Decisión\n```{r}\n", cod.dt.pred,
                           "\nhead(tb_predic(real.val, prediccion.dt)x$data)\n```"))

      plot_disp_dt()
      nombres.modelos <<- c(nombres.modelos, "prediccion.dt")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_dt(2)
      showNotification(paste0("Error (DT-02) : ",e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_dt_ind <- function() {
    if(exists("prediccion.dt")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.dt.ind))
        
        indices.dt <<- general.indices(datos.prueba[,variable.predecir], prediccion.dt)
        
        insert.report("ind.dt", paste0("## Índices Generales \n```{r}\n", cod.dt.ind, "\ngeneral.indices(datos.prueba[,'",variable.predecir,"'], prediccion.dt)\n",
                                       "indices.dt <- general.indices(datos.prueba[,'",variable.predecir,"'], prediccion.dt)\n",
                                       "IndicesM[['dtl']] <<- indices.dt\n```"))

        df <- as.data.frame(indices.dt)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfdt <- render.index.table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfdt2 <- render.index.table(df2)
        
        IndicesM[["dtl"]] <<- indices.dt
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_dt(3)
        showNotification(paste0("Error (DT-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # RF PAGE ---------------------------------------------------------------------------------------------------------------

  # When the rf model is generated
  observeEvent(input$runRf, {
    if (validate_data()) { # Si se tiene los datos entonces :
      rf_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$ntree.rf,input$mtry.rf), {
    if (validate_data(print = FALSE) & rf.stop.excu) {
      deafult_codigo_rf()
    }else{
      rf.stop.excu <<- TRUE
    }
  })

  # When user change the rule selector
  observeEvent(input$rules.rf.n,{
    if(validate_data(print = FALSE)){
        show_rf_rules(input$rules.rf.n)
    }
  })

  # Upgrade code fields to default version
  deafult_codigo_rf <- function(rf.def = FALSE){
    if(!is.null(datos.aprendizaje) & rf.def){
      mtry.value <- ifelse(rf.def, round(sqrt(ncol(datos.aprendizaje))), input$mtry.rf)
      updateNumericInput(session,"mtry.rf",value = mtry.value)
    }else{
      mtry.value <- input$mtry.rf
    }

    # Se acualiza el codigo del modelo
    codigo <- rf.modelo(variable.pr = variable.predecir,
                        ntree = input$ntree.rf,
                        mtry = mtry.value)

    updateAceEditor(session, "fieldCodeRf", value = codigo)
    cod.rf.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- rf.prediccion(variable.predecir)
    updateAceEditor(session, "fieldCodeRfPred", value = codigo)
    cod.rf.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- rf.disp()
    updateAceEditor(session, "fieldCodeRfDisp", value = codigo)

    # Cambia el codigo del grafico de rf
    updateAceEditor(session, "fieldCodeRfPlot", value = extract.code("importance.plor.rf"))

    # Se genera el codigo de la indices
    codigo <- extract.code("general.indices")
    updateAceEditor(session, "fieldCodeRfIG", value = codigo)
    cod.rf.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_rf <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rf <<- NULL
        output$txtRf <- renderPrint(invisible(""))
        remove.report.elem("modelo.rf")
        remove.report.elem("modelo.rf.graf")
        remove.report.elem("disp.rf")
      }, {
        prediccion.rf <<- NULL
        remove.report.elem("pred.rf")
        output$rfPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rf <<- rep(0, 10)
        remove.report.elem("ind.rf")
      })
    }
  }

  # Shows the chart of importance - OJO
  plotear_rf_imp <- function() {
    tryCatch({
      output$plot.rf <- renderPlot(isolate(importance.plor.rf(modelo.rf,translate("impVarA"),translate("impVarRSS"))))
      cod <- ifelse(input$fieldCodeRfPlot == "", extract.code("importance.plor.rf"), input$fieldCodeRfPlot)
      insert.report("modelo.rf.graf", paste0("## Importancia de las Variables\n```{r}\n", cod , "\n```"))
    }, error = function(e) {
      output$plot.rf <- renderPlot(NULL)
      insert.report("modelo.rf.graf",NULL)
    })
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rf <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.rf.disp <- renderPlot(exe(input$fieldCodeRfDisp))
      insert.report("disp.rf",
                    paste0("## Dispersión del Modelo RF\n```{r}\n", input$fieldCodeRfDisp,"\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rf(2)
      showNotification(paste0("Error (RF-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Show Rules
  show_rf_rules <- function(n){
    output$rulesRf <- renderPrint({
      tryCatch({
          updateAceEditor(session,"fieldCodeRfRules",paste0("printRandomForests(modelo.rf, ",n,")"))
          printRandomForests(modelo.rf, n)
        },error = function(e){
          stop(translate("NoDRule"))
      })
    })
    insert.report(paste0("modelo.rf.rules.", n), paste0("\n## Reglas del árbol #",n," \n```{r}\nprintRandomForests(modelo.rf, ",n,")\n```"))
  }

  # Execute model, prediction and indices
  rf_full <- function(){
    execute_rf()
    execute_rf_pred()
    execute_rf_ind()
  }

  # Generates the model
  execute_rf <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rf.modelo))
      output$txtRf <- renderPrint(print(modelo.rf))

      insert.report("modelo.rf",paste0("## Generación del Modelo Bosques Aleatorios\n```{r}\n",
                                       cod.rf.modelo, "\nmodelo.rf\n```"))

      plotear_rf_imp()
      plot_disp_rf()
      show_rf_rules(input$rules.rf.n)
      nombres.modelos <<- c(nombres.modelos, "modelo.rf")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rf(1)
      showNotification(paste0("Error (RF-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_rf_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rf.pred))

      output$rfPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.rf), server = FALSE)

      insert.report("pred.rf",
                    paste0("## Predicción del Modelo Bosques Aleatorios\n```{r}\n", cod.rf.pred,
                           "\nhead(tb_predic(real.val, prediccion.rf)x$data)\n```"))

      nombres.modelos <<- c(nombres.modelos, "prediccion.rf")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rf(2)
      showNotification(paste0("Error (RF-02) : ", e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_rf_ind <- function() {
    if(exists("prediccion.rf")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rf.ind))

        indices.rf <<- general.indices(datos.prueba[,variable.predecir], prediccion.rf)

        insert.report("ind.rf",paste0("## Índices Generales\n```{r}\n",
                                      cod.rf.ind, "\ngeneral.indices(datos.prueba[,'",variable.predecir,"'], prediccion.rf)\n",
                                      "indices.rf <- general.indices(datos.prueba[,'",variable.predecir,"'], prediccion.rf)\n",
                                      "IndicesM[['rfl']] <<- indices.rf\n```"))

        df <- as.data.frame(indices.rf)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrf <- render.index.table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrf2 <- render.index.table(df2)

        nombres.modelos <<- c(nombres.modelos, "indices.rf")
        IndicesM[["rfl"]] <<- indices.rf
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_rf(3)
        showNotification(paste0("Error (RF-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # BOOSTING PAGE ---------------------------------------------------------------------------------------------------------

  # When the boosting model is generated
  observeEvent(input$runBoosting, {
    if (validate_data()){ # Si se tiene los datos entonces :
      boosting_full()
    }
  })

  # When user change the rule selector
  observeEvent(input$rules.b.n,{
    if(validate_data(print = FALSE)){
      mostrar.reglas.boosting(input$rules.b.n)
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$iter.boosting, input$nu.boosting, input$tipo.boosting, input$shrinkage.boosting, input$maxdepth.boosting), {
    if (validate_data(print = FALSE)){
      deault_codigo_boosting()
    }
  })

  # Upgrade code fields to default version
  deault_codigo_boosting <- function() {
    # Se acualiza el codigo del modelo
    codigo <- boosting.modelo(variable.pr = variable.predecir,
                              iter = input$iter.boosting,
                              type = input$tipo.boosting,
                              minsplit = input$shrinkage.boosting)

    updateAceEditor(session, "fieldCodeBoosting", value = codigo)
    cod.b.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- boosting.prediccion(variable.predecir, input$tipo.boosting)
    updateAceEditor(session, "fieldCodeBoostingPred", value = codigo)
    cod.b.pred <<- codigo

    # Se genera el codigo de la dispersion
    codigo <- boosting.disp(input$tipo.boosting)
    updateAceEditor(session, "fieldCodeBoostingDisp", value = codigo)
    
    # Cambia el codigo del grafico de importancia
    updateAceEditor(session, "fieldCodeBoostingPlotImport", value = boosting.plot.import(input$tipo.boosting))

    # Se genera el codigo de la indices
    codigo <- extract.code("general.indices")
    updateAceEditor(session, "fieldCodeBoostingIG", value = codigo)
    cod.b.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_boosting <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        exe("modelo.boosting.",input$tipo.boosting," <<- NULL")
        output$txtBoosting <- renderPrint(invisible(""))
        output$plot.boosting.import <- renderPlot(NULL)
        remove.report.elem(paste0("modelo.b.",input$tipo.boosting))
        remove.report.elem(paste0("modelo.b.error.",input$tipo.boosting))
        remove.report.elem(paste0("modelo.b.imp.",input$tipo.boosting))
      }, {
        exe("prediccion.boosting.",input$tipo.boosting," <<- NULL")
        remove.report.elem(paste0("pred.b.",input$tipo.boosting))
        output$boostingPrediTable <- DT::renderDataTable(NULL)
      },{
        exe("indices.boosting.",input$tipo.boosting," <<- NULL")
        remove.report.elem(paste0("ind.b.",input$tipo.boosting))
      })
    }
  }

  # Shows the chart of importance - OJO
  plotear_boosting_imp <- function() {
    tryCatch({
      codigo <- input$fieldCodeBoostingPlotImport
      tipo <- input$tipo.boosting
      output$plot.boosting.import <- renderPlot(isolate(exe(codigo)))
      cod <- ifelse(codigo == "",boosting.plot.import(), codigo)
      insert.report(paste0("modelo.b.imp.",tipo),paste0("## Importancia de las Variables - ",tipo,"\n```{r}\n", cod , "\n```"))
    }, error = function(e) {
      clean_boosting(1)
    })
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_boosting <- function(){
    tryCatch({ # Se corren los codigo
      tipo <- input$tipo.boosting
      codigo <- input$fieldCodeBoostingDisp
      output$plot.boosting.disp <- renderPlot(exe(codigo))
      insert.report(paste0("disp.boosting.",tipo),
                    paste0("## Dispersión del Modelo BOOSTING - ",tipo,"\n```{r}\n", codigo ,"\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(2)
      showNotification(paste0("Error (B-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  boosting_full <- function() {
    if(!is.null(calibrar.boosting())){
      execute_boosting()
      execute_boosting_pred()
      execute_boosting_ind()
    }else{
      showNotification(translate("ErrorBsize"), duration = 15, type = "error")
    }
  }

  # Generates the model
  execute_boosting <- function() {
    tryCatch({ # Se corren los codigo
        isolate(exe(cod.b.modelo))
        isolate(tipo <- input$tipo.boosting)
        output$txtBoosting <- renderPrint(exe("print(summary(modelo.boosting.",tipo,",plotit = FALSE))"))
        
        plotear_boosting_imp()
  
        insert.report(paste0("modelo.b.",tipo),
                      paste0("## Generación del Modelo BOOSTING - ",tipo,"\n```{r}\n",
                             cod.b.modelo, "\nmodelo.boosting.",tipo,"\n```"))
  
        nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(1)
      showNotification(paste0("Error (B-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_boosting_pred <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.b.pred))
      isolate(tipo <- input$tipo.boosting)
      # Cambia la tabla con la prediccion de boosting
      output$boostingPrediTable <- DT::renderDataTable(tb_predic(real.val, exe("prediccion.boosting.",tipo)),server = FALSE)
      insert.report(paste0("pred.b.",tipo),
                    paste0("## Predicción del Modelo BOOSTING - ",tipo,"\n```{r}\n",
                    cod.b.pred,"\nhead(tb_predic(real.val, prediccion.boosting.",input$tipo.boosting,")x$data)\n```"))

      plot_disp_boosting()
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(2)
      showNotification(paste0("Error (B-02) : ",e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_boosting_ind <- function() {
    if(exists(paste0("prediccion.boosting.",input$tipo.boosting))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.b.ind))
        isolate(tipo <- input$tipo.boosting)
        
        indices.boosting <- general.indices(datos.prueba[,variable.predecir], exe("prediccion.boosting.",tipo))
        eval(parse(text = paste0("indices.boosting.",tipo, "<<- indices.boosting")))

        insert.report(paste0("ind.b.",tipo),
                      paste0("## Índices Generales del Modelo BOOSTING - ",tipo,"\n```{r}\n",
                             cod.knn.ind, "\ngeneral.indices(datos.prueba[,'",variable.predecir,"'] ,prediccion.boosting.",tipo,")\n",
                             "indices.boosting <- general.indices(datos.prueba[,'",variable.predecir,"'], prediccion.boosting.",tipo,")\n",
                             "IndicesM[['bl-",tipo,"']] <<- indices.boosting\n```"))
        
        df <- as.data.frame(indices.boosting)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfb <- render.index.table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfb2 <- render.index.table(df2)

        nombres.modelos <<- c(nombres.modelos, paste0("indices.boosting.",tipo))
        IndicesM[[paste0("bl-",tipo)]] <<- exe("indices.boosting.",tipo)
        actualizar.selector.comparativa()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_boosting(3)
        showNotification(paste0("Error (B-03) : ", e), duration = 15, type = "error")
      })
    }
  }
  
  # NN PAGE ---------------------------------------------------------------------------------------------------------------

  # When user change the layer selector
  observeEvent(c(input$cant.capas.nn, input$segmentButton), {
    if(!is.null(datos.aprendizaje) && !is.null(input$cant.capas.nn)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn) {
          shinyjs::show(paste0("nn.cap.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.", i))
        }
      }
    }
  })

  # When the nn model is generated
  observeEvent(input$runNn, {
    if (validate_data()) { # Si se tiene los datos entonces :
      nn_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$cant.capas.nn,input$threshold.nn,input$stepmax.nn,
                 input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,input$nn.cap.5,
                 input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,input$nn.cap.9,input$nn.cap.10),{
    if(validate_data(print = FALSE)){
      default_codigo_nn()
    }
  })

  # Upgrade code fields to default version
  default_codigo_nn <- function(){
    
    #Se acualiza el codigo del modelo
    codigo <- nn.modelo(input$threshold.nn,
                        input$stepmax.nn,
                        input$cant.capas.nn,
                        input$nn.cap.1,input$nn.cap.2,
                        input$nn.cap.3,input$nn.cap.4,
                        input$nn.cap.5,input$nn.cap.6,
                        input$nn.cap.7,input$nn.cap.8,
                        input$nn.cap.9,input$nn.cap.10)

    updateAceEditor(session, "fieldCodeNn", value = codigo)
    cod.nn.modelo <<- codigo

    #Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeNnPlot", value = nn.plot())

    #Se genera el codigo de la prediccion
    codigo <- nn.prediccion()
    updateAceEditor(session, "fieldCodeNnPred", value = codigo)
    cod.nn.pred <<- codigo

    # Se genera el codigo de la dispersion
    codigo <- nn.disp()
    updateAceEditor(session, "fieldCodeNnDisp", value = codigo)

    #Se genera el codigo de la indices
    codigo <- extract.code("general.indices")
    updateAceEditor(session, "fieldCodeNnIG", value = codigo)
    cod.nn.ind <<- codigo
  }

  # Shows the graph of the network
  plot_net <- function(){
    tryCatch({
      capas <- c(input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,
                 input$nn.cap.5,input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,input$nn.cap.9,input$nn.cap.10)
      capas <- capas[1:input$cant.capas.nn]
      if(input$cant.capas.nn * sum(capas) <= 1500 & ncol(modelo.nn$covariate) <= 20){
        output$plot.nn <- renderPlot(isolate(exe(input$fieldCodeNnPlot)))
        cod <- ifelse(input$fieldCodeNnPlot == "", nn.plot(), input$fieldCodeNnPlot)
        insert.report("modelo.nn.graf", paste0("\n```{r}\n", cod, "\n```"))
      }else{
        showNotification(translate("bigPlot"), duration = 10, type = "message")
      }
    },
    error = function(e){
      output$plot.nn <- renderPlot(NULL)
      remove.report.elem("modelo.nn.graf")
    })
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_nn <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.nn.disp <- renderPlot(exe(input$fieldCodeNnDisp))
      insert.report("disp.nn",
                    paste0("## Dispersión del Modelo NN\n```{r}\n", input$fieldCodeNnDisp,"\n```\n"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_nn(2)
      showNotification(paste0("Error (NN-02) : ", e), duration = 15, type = "error")
    })
  }

  # Cleans the data according to the process where the error is generated
  clean_nn <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        modelo.nn <<- NULL
        output$txtnn <- renderPrint(invisible(""))
        output$plot.nn <- renderPlot(NULL)
        remove.report.elem("modelo.nn")
        remove.report.elem("modelo.nn.graf")
      }, {
        prediccion.nn <<- NULL
        remove.report.elem("pred.nn")
        output$nnPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.nn <<- rep(0, 10)
        remove.report.elem("ind.nn")
      })
    }
  }

  # Execute model, prediction and indices
  nn_full <- function() {
    execute_nn()
    if(NN_EXECUTION){
      execute_nn_pred()
      execute_nn_ind()
    }
  }

  # Generates the model
  execute_nn <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.nn.modelo))
      output$txtnn <- renderPrint(print(modelo.nn))
      insert.report("modelo.nn",
                    paste0("## Generación del modelo Redes Neuronales\n```{r}\n", cod.nn.modelo,
                           "\nsummary(modelo.nn)\n```"))
      plot_net()
      nombres.modelos <<- c(nombres.modelos,"modelo.nn")
      NN_EXECUTION <<- TRUE
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_nn(1)
      showNotification(paste0("Error (NN-01) : ",e), duration = 15, type = "error")
    },
    warning = function(w){
      clean_nn(1)
      NN_EXECUTION <<- FALSE
      showNotification(paste0(translate("nnWar")," (NN-01) : ",w), duration = 20, type = "warning")
    })
  }

  # Generate the prediction
  execute_nn_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.nn.pred))
      
      # Cambia la tabla con la prediccion de nn
      output$nnPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.nn),server = FALSE)

      insert.report("pred.nn",
                    paste0("## Predicción del Modelo Redes Neuronales\n```{r}\n", cod.nn.pred,
                           "\nhead(tb_predic(real.val, prediccion.nn)x$data)\n```"))

      plot_disp_nn()
      nombres.modelos <<- c(nombres.modelos,"prediccion.nn")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comparativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_nn(2)
      showNotification(paste0("Error (NN-02) : ",e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_nn_ind <- function() {
    if(exists("prediccion.nn")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.nn.ind))
        indices.nn <<- general.indices(datos.prueba[,variable.predecir], prediccion.nn)

        insert.report("ind.nn", paste0("## Índices Generales \n```{r}\n", cod.nn.ind, 
                                       "\ngeneral.indices(datos.prueba[,'",variable.predecir,"'], prediccion.nn)\n",
                                       "indices.nn <- general.indices(datos.prueba[,'",variable.predecir,"'], prediccion.nn)\n",
                                       "IndicesM[['nn']] <<- indices.nn\n```"))

        df <- as.data.frame(indices.nn)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfnn <- render.index.table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfnn2 <- render.index.table(df2)
        
        IndicesM[["nn"]] <<- indices.nn
        actualizar.selector.comparativa()
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        clean_nn(3)
        showNotification(paste0("Error (NN-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # COMPARISON CHART ------------------------------------------------------------------------------------------------------
  
  # Updates the selectors in the comparison table page
  actualizar.selector.comparativa <- function(){
    nombres <- models_mode()
    shinyWidgets::updateCheckboxGroupButtons(session,"select.models",choices = sort(nombres),selected = sort(nombres),
                                             status = "primary",checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                                 no = icon("remove", lib = "glyphicon")))
  }

  #Muestra la tabla comparativa.
  output$TablaComp <- DT::renderDataTable({
    graficar <- updatePlot$tablaCom
    if (!is.null(datos.aprendizaje)) {
      insert.report("tabla.comparativa",paste0("## Tabla Comparativa \n```{r}\ncomparative.table( ",
                                               as.string.c(input$select.models),")\n```"))
      DT::datatable(comparative.table(input$select.models),
                    selection = "none", editable = FALSE,
                    options = list(dom = "frtip", pageLength = 9, buttons = NULL))
    }
  },server = FALSE)

  # NEW PREDICTIONS PAGE --------------------------------------------------------------------------------------------------

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

  crear.datos.np <- function(){
    datos.aux.prueba <- datos.prueba.completos
    datos.aux.prueba[,variable.predecir.pn] <- predic.nuevos
    return(datos.aux.prueba)
  }

  actualizar.pred.pn <- function(codigo){
    updateAceEditor(session, "fieldCodePredPN", value = codigo)
    if(!is.null(predic.nuevos)){
      datos.aux.prueba <- crear.datos.np()
      output$PrediTablePN <- render_table_data(datos.aux.prueba,editable = F,
                                                    scrollY = "25vh",server = T)
    }else{
      output$PrediTablePN <- DT::renderDT(DT::datatable(data.frame()))
    }
  }

  actualizar.nn.capas.np <- function(){
    if(!is.null(datos.aprendizaje.completos) && !is.null(input$cant.capas.nn.pred)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn.pred) {
          shinyjs::show(paste0("nn.cap.pred.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.pred.", i))
        }
      }
    }
  }

  actualizar.nn.capas.np()

  # Download the data with the prediction
  output$downloaDatosPred <- downloadHandler(
    filename = function() {
      input$file3$name
    },
    content = function(file) {
      if(!is.null(predic.nuevos)){
        write.csv(crear.datos.np(), file, row.names = input$rownameNPred2)
      }
    }
  )

  observeEvent(c(input$cant.capas.nn.pred), {
    actualizar.nn.capas.np()
  })

  observeEvent(input$permitir.landa.pred, {
    if (input$permitir.landa.pred) {
      shinyjs::enable("landa.pred")
    } else {
      shinyjs::disable("landa.pred")
    }
  })
  
  observeEvent(input$loadButtonNPred,{
    codigo.carga <- code.carga(nombre.filas = input$rownameNPred,
                               ruta = input$file2$datapath,
                               separador = input$sepNPred,
                               sep.decimal = input$decNPred,
                               encabezado = input$headerNPred,
                               d.o = "datos.originales.completos",
                               d = "datos.aprendizaje.completos")

    tryCatch({
      isolate(exe(codigo.carga))
      if(ncol(datos.originales.completos) <= 1) {
        showNotification(translate("errorSeg"), duration = 10, type = "error")
        return(NULL)
      }
      codigo.na <- ""
      codigo.na <- paste0(code.NA(deleteNA = input$deleteNAnPred, d.o = "datos.originales.completos"), "\n", "datos.aprendizaje.completos <<- datos.originales.completos")
      isolate(exe( codigo.na))

      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.numerical(datos.aprendizaje.completos))))
      updateNumericInput(session, "kmax.knn.pred", value = round(sqrt(nrow(datos.aprendizaje.completos))))
      updateNumericInput(session, "mtry.rf.pred", value = round(sqrt(ncol(datos.aprendizaje.completos) -1)))
    },
    error = function(e) {
      showNotification(paste0("Error:", e), duration = 10, type = "error")
      datos.aprendizaje.completos <<- NULL
      datos.originales.completos <<- NULL
      return(NULL)
    })

    modelo.nuevos <<- NULL
    predic.nuevos <<- NULL
    actualizar.pred.pn("")

    update_model_text_pn("")
    update_table_pn()
  })

  update.trans.pn <- eventReactive(c(input$loadButtonNPred), {
    contadorPN <<- contadorPN + 1
    if (!is.null(datos.aprendizaje.completos) && ncol(datos.aprendizaje.completos) > 0) {
      res <- data.frame(Variables = colnames(datos.aprendizaje.completos),
                        Tipo = c(1:ncol(datos.aprendizaje.completos)),
                        Activa = c(1:ncol(datos.aprendizaje.completos)))
      res$Tipo <- sapply(colnames(datos.aprendizaje.completos), function(i) paste0(
        '<select id="Predsel', i, contadorPN, '"> <option value="categorico">',translate("categorico"),'</option>',
        '<option value="numerico" ', ifelse(class(datos.aprendizaje.completos[, i]) %in% c("numeric", "integer"),' selected="selected"', ""),'>', translate("numerico"),'</option>',
        '<option value="disyuntivo">',translate("disyuntivo"),'</option> </select>'
      ))
      res$Activa <- sapply(colnames(datos.aprendizaje.completos), function(i) paste0('<input type="checkbox" id="Predbox', i, contadorPN, '" checked/>'))
      actualizar.nn.capas.np()
    } else {
      res <- as.data.frame(NULL)
      showNotification(translate("tieneCData"), duration = 10, type = "error")
    }
    return(res)
  })

  output$transDataPredN <- DT::renderDataTable(update.trans.pn(),
                                          escape = FALSE, selection = "none", server = FALSE,
                                          options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = "35vh"), rownames = F,
                                          callback = JS("table.rows().every(function(i, tab, row) {
                                                        var $this = $(this.node());
                                                        $this.attr('id', this.data()[0]);
                                                        $this.addClass('shiny-input-checkbox');});
                                                        Shiny.unbindAll(table.table().node());
                                                        Shiny.bindAll(table.table().node());"))

  transformar.datos.pn <- function() {
    var.noactivas <- c()
    code.res <- "datos.aprendizaje.completos <<- datos.originales.completos \n"
    for (var in colnames(datos.originales.completos)) {
      if (input[[paste0("Predbox", var, contadorPN)]]) {
        if (input[[paste0("Predsel", var, contadorPN)]] == "categorico" & class(datos.originales.completos[, var]) %in% c("numeric", "integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico", d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
        if (input[[paste0("Predsel", var, contadorPN)]] == "numerico" & !(class(datos.originales.completos[, var]) %in% c("numeric", "integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico",  d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
        if (input[[paste0("Predsel", var, contadorPN)]] == "disyuntivo") {
          code.res <- paste0(code.res, code.trans(var, "disyuntivo", d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }


    isolate(exe(code.res))
    if (length(var.noactivas) > 0) {
      des <- code.desactivar(var.noactivas,"datos.aprendizaje.completos")
      isolate(exe(des))
      code.res <- paste0(code.res, "\n", des)
    }
    code.res <- paste0(code.res, "\n")
    return(code.res)
  }

  predecir.pn <-function(){
    if(!is.null(datos.prueba.completos)){
      if(exists("modelo.nuevos") && !is.null(modelo.nuevos)){
        codigo <- switch(modelo.seleccionado.pn,
                         rl  =  rl.prediccion.np(),
                         rlr =  rlr.prediccion.np(alpha = input$alpha.rlr.pred,
                                                  escalar = input$switch.scale.rlr.pred,
                                                  manual = input$permitir.landa.pred,
                                                  landa = input$landa.pred),
                         knn =  kkn.prediccion.pn(),
                         dt  = dt.prediccion.np(),
                         rf  = rf.prediccion.np(),
                         boosting = boosting.prediccion.np(),
                         svm = svm.prediccion.np(),
                         nn = nn.prediccion.np())
        tryCatch({
          exe(codigo)
          actualizar.pred.pn(codigo)
        },error =  function(e){
            showNotification(paste0("Error :", e), duration = 10, type = "error")
        })
      }else{
        showNotification(paste0("Error :", translate("ErrorModelo")), duration = 10, type = "error")
      }
    }else{
      showNotification(paste0("Error :", translate("ErrorDatosPN")), duration = 10, type = "error")
    }
  }

  observeEvent(input$predecirPromidat, {
    predecir.pn()
  })
  
  observeEvent(input$transButtonPredN, {
    # transforma los datos
    code.trans.pn <<- transformar.datos.pn()

    # Actualiza los selectores que dependen de los datos
    updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.numerical(datos.aprendizaje.completos))))
    updateNumericInput(session, "mtry.rf.pred", value = round(sqrt(ncol(datos.aprendizaje.completos) -1)))

    modelo.nuevos <<- NULL
    predic.nuevos <<- NULL
    actualizar.pred.pn("")

    update_model_text_pn("")
    update_table_pn()
  })

  observeEvent(input$PredNuevosBttnModelo,{
    variable.predecir.pn <<- input$sel.predic.var.nuevos
    modelo.seleccionado.pn  <<- input$selectModelsPred
    
    codigo <- switch(input$selectModelsPred,
                     rl  = rl.modelo.np(),
                     rlr  = rlr.modelo.np(alpha = input$alpha.rlr.pred,
                                          escalar = input$switch.scale.rlr.pred,
                                          manual = input$permitir.landa.pred,
                                          landa = input$landa.pred),
                     knn =  kkn.modelo.np(scale = input$switch.scale.knn.pred,
                                          kmax = input$kmax.knn.pred,
                                          kernel = input$kernel.knn.pred),
                     dt  = dt.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                        minsplit = input$minsplit.dt.pred,
                                        maxdepth = input$maxdepth.dt.pred),
                     rf  = rf.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                        ntree = input$ntree.rf.pred,
                                        mtry = input$mtry.rf.pred),
                     boosting = boosting.modelo.np(variable.pr = input$sel.predic.var.nuevos,
                                              iter = input$iter.boosting.pred,
                                              type = input$tipo.boosting.pred,
                                              minsplit = input$shrinkage.boosting.pred),
                     svm = svm.modelo.np(scale = input$switch.scale.svm.pred,
                                         kernel = input$kernel.svm.pred),
                     nn = nn.modelo.np(variable.pr=input$sel.predic.var.nuevos,
                                        input$threshold.nn.pred,
                                        input$stepmax.nn.pred,
                                        input$cant.capas.nn.pred,
                                        input$nn.cap.pred.1,input$nn.cap.pred.2,
                                        input$nn.cap.pred.3,input$nn.cap.pred.4,
                                        input$nn.cap.pred.5,input$nn.cap.pred.6,
                                        input$nn.cap.pred.7,input$nn.cap.pred.8,
                                        input$nn.cap.pred.9,input$nn.cap.pred.10))

      modelo.nuevos <<- NULL
      predic.nuevos <<- NULL
      actualizar.pred.pn("")
      
      tryCatch({
        if( (input$selectModelsPred == "boosting" && !is.null(calibrar.boosting.np()) ) || input$selectModelsPred != "boosting" ){
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

  observeEvent(input$loadButtonNPred2,{
    codigo.carga <- code.carga( nombre.filas = input$rownameNPred2,
                                ruta = input$file3$datapath,
                                separador = input$sep.nPred2,
                                sep.decimal = input$dec.nPred2,
                                encabezado = input$headerNPred2,
                                d.o = "datos.prueba.completos",
                                d = "datos.prueba.completos")

    tryCatch({
      isolate(exe(codigo.carga))
      codigo.na <- ""
      codigo.na <- paste0(code.NA(deleteNA = input$deleteNAnPred2,
                                  d.o = paste0("datos.prueba.completos")))
      datos.prueba.completos[,variable.predecir.pn] <<- NULL
      validate_pn_data()
      isolate(exe( codigo.na))
      datos.prueba.completos[,variable.predecir.pn] <<- NA
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

  # PAGINA DE REPORTE -------------------------------------------------------------------------------------------------------

  len.report <- function(){
    length(env.report$codigo.reporte)
  }

  remove.report.elem <- function(id){
    insert.report(id, NULL)
  }

  insert.report <- function(id, content, interpretation =  TRUE){
    n <- len.report()
    if(is.null(content)){
      env.report$codigo.reporte[[n]][[id]] <<- content
    }else{
      env.report$codigo.reporte[[n]][[id]] <<- ifelse(interpretation, paste0(content,"\n\n#### Interpretación\n\n"), content)
    }
  }

  names.report <- function(){
    n <- len.report()
    names(env.report$codigo.reporte[[n]])
  }

  new.report <- function(){
    n <- len.report() + 1
    env.report$codigo.reporte[[n]] <<- list(datos.originales = datos.originales)
    env.report$codigo.reporte[[n]][["carga.datos"]] <<- paste0("\n# Carga de Datos (",input$file1$name,")",
                                                    "\n```{r}\ndatos.originales <<- codigo.reporte[[",n,"]]$datos.originales\n",
                                                    "datos <<- datos.originales\n```\n```{r}\nhead(datos)\n```\n```{r}\nstr(datos)\n```\n",
                                                    "```{r}\nIndicesM <<- list()\n```\n")
  }

  new.secction.report <- function(){
    n <- len.report() + 1
    env.report$codigo.reporte[[n]] <<- list()
  }

  observeEvent(input$principal, {
    if(input$principal == "reporte"){
      updateAceEditor(session, "fieldCodeReport", value = def.reporte(titulo = input$textTitulo, nombre = input$textNombre, input))
    }
  })

  observeEvent(input$textTitulo, {
    updateAceEditor(session, "fieldCodeReport", value = str_replace(input$fieldCodeReport, "title: '.*'", paste0("title: '", input$textTitulo, "'")))
  })

  observeEvent(input$textNombre, {
    updateAceEditor(session, "fieldCodeReport", value = str_replace(input$fieldCodeReport, "author: '.*'", paste0("author: '", input$textNombre, "'")))
  })

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
      options(encoding = enc) #Variable global
      write.table(input$fieldCodeReport,namermd,row.names=F,col.names=F,quote=F)
      options(encoding = e)

      files <- c(namermd, files)

      src <- normalizePath(namermd)
      withCallingHandlers({
        overwrite_cat()
        salida.code <<- ""
        shinyjs::html("txtreport", salida.code)
        out <- rmarkdown::render(src,  params = NULL, rmarkdown::word_document(highlight = "tango"), envir = env.report)
      },
      message = function(m) {
        salida.code <<- paste0(m$message, salida.code)
        shinyjs::html(id = "txtreport", html = salida.code)
      })

      recover.cat()
      file.rename(out, paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''))
      files <- c(paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''), files)

      zip::zip(file, files)
    }
  )

  # CAMBIAR IDIOMA ----------------------------------------------------------------------------------------------------------

  #Elimina NULLs
  dropNulls <- function (x) {
    x[!vapply(x, is.null, FUN.VALUE = logical(1))]
  }

  updateLabelInput <- function (session, labelid, value = NULL) {
    message <- dropNulls(list(labelid = labelid))
    if(length(labelid) == 1) {
      labelid <- list(labelid)
    }
    ifelse(is.null(value), sentvalue <- translate(labelid),
            ifelse(length(value) == 1, sentvalue <- list(value), sentvalue <- value))
    session$sendCustomMessage(type = 'updateLabel',
                              message = list(ids = labelid, values = sentvalue))
  }

  observeEvent(c(input$idioma), {
    updateLabelInput(session, c("idioma","selidioma","data","basico","resumen","normalidad",
                                "dispersion","distribucion","correlacion","poderpred","reporte",
                                "aprendizaje","acercade","comparacion","predicnuevos","knnl","dtl",
                                "rfl","bl","svml","cargar","header","Rownames","eliminana","si","no",
                                "cargarchivo","subir","trans","aplicar","separador","coma","puntocoma",
                                "tab","separadordec","punto","subir","configuraciones","semilla",
                                "habilitada","deshabilitada","seleccionarPredecir","propA","propP",
                                "generar","descargar","dataA","dataP","numerico","categorico","disyuntivo",
                                "resumenvar","selvar","plotnormal","opciones", "selcolor","selvars",
                                "selcolores","codigo","codedist","numericas","categoricas","ejecutar",
                                "selmetodo","seltipo","resultados","distpred","distpredcat","pares",
                                "denspred","generatem","predm","mc","indices","gclasificacion","garbol",
                                "reglas","evolerror","varImp","selkernel","kmax","escal","minsplit",
                                "maxdepth","splitIndex","numTree","numVars","ruleNumTree","selectAlg",
                                "rocCurva","tablaComp","selectMod","selectCat", "reporte","titulo",
                                "nombre","codreporte","salida","copyright","info","version","cargarNuev",
                                "cargarDatos","transDatos","seleParModel","generarM","variables","tipo",
                                "activa","nn","xgb","selbooster","selnrounds","selectCapas","threshold",
                                "stepmax","redPlot","rll","rlr","posibLanda","coeff","gcoeff",
                                "automatico","landa","shrinkage","resumenVarPre", "R2"))

    updatePlot$normal <- default.normal("datos", input$sel.normal, input$col.normal, translate("curvanormal"))
    updatePlot$dya.cat <- def.code.cat(variable = input$sel.distribucion.cat, titulox = translate("cantidadcasos"), tituloy = translate("categorias"))
    updatePlot$calc.normal <- default.calc.normal(labelsi = translate("positivo"),labelno=translate("negativo"),labelsin=translate("sinasimetria"))

    execute_knn_ind()
    execute_svm_ind()
    execute_dt_ind()
    execute_rf_ind()
    execute_rl_ind()
  })

  # TERMINA LA SESION -------------------------------------------------------------------------------------------------------

  # When the session closes
  session$onSessionEnded(function() {
    rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
    unlink("figure", recursive = T)
    recover_cat()
    stopApp()
  })

})
