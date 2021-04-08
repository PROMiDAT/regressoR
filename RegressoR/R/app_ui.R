#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  suppressMessages(suppressWarnings({
    library(DT)
    library(gbm)
    library(kknn)
    library(shiny)
    library(e1071)
    library(rpart)
    library(knitr)
    library(glmnet)
    library(rattle)
    library(xtable)
    library(xgboost)
    library(shinyjs)
    library(ggplot2)
    library(stringr)
    library(forcats)
    library(shinyAce)
    library(corrplot)
    library(neuralnet)
    library(rpart.plot)
    library(randomForest)
    library(colourpicker)
    library(shinyWidgets)
    library(flexdashboard)
    library(shinydashboard)
    library(shinydashboardPlus)
    library(dplyr)
    library(zip)
    library(pls)
  }))
  
  # MENU --------------------------------------------------------------------------------------------------------------------
  load.menu <- menuItem(labelInput("data"), tabName = "cargar", icon = icon("dashboard"))

  
  statistics.menu <- menuItem(labelInput("basico"), tabName = "parte1", icon = icon("th-list"),
                              menuSubItem(labelInput("resumen"), tabName = "resumen", icon = icon("sort-numeric-asc")),
                              menuSubItem(labelInput("normalidad"), tabName = "normalidad", icon = icon("bar-chart")),
                              menuSubItem(labelInput("dispersion"), tabName = "dispersion", icon = icon("line-chart")),
                              menuSubItem(labelInput("distribucion"), tabName = "distribucion", icon = icon("area-chart")),
                              menuSubItem(labelInput("correlacion"), tabName = "correlacion", icon = icon("table")),
                              menuItem(labelInput("poderpred"), tabName = "poderPred", icon = icon("rocket")))
  
  supervised.learning.menu    <- menuItem(labelInput("aprendizaje"), tabName = "parte2", icon = icon("th-list"),
                                          menuSubItem(labelInput("rll"),tabName = "rl",icon = icon("line-chart")),
                                          menuSubItem(labelInput("rlr"),tabName = "rlr",icon = icon("line-chart")),
                                          menuSubItem(labelInput("dtl"),tabName = "dt",icon = icon("tree")),
                                          menuSubItem(labelInput("rfl"),tabName = "rf",icon = icon("sitemap")),
                                          menuSubItem(labelInput("bl"),tabName = "boosting",icon = icon("superscript")),
                                          menuSubItem(labelInput("knnl"),tabName = "knn",icon = icon("dot-circle-o")),
                                          menuSubItem(labelInput("svml"),tabName = "svm",icon = icon("line-chart")),
                                          menuSubItem(labelInput("rd"), tabName = "rd",icon = icon("chart-pie")),
                                          menuSubItem(labelInput("nn"),tabName = "nn",icon = icon("brain")))
  
  report.menu <- menuItem(labelInput("reporte"), tabName = "reporte", icon = icon("save-file",lib = "glyphicon"))
  
  compare.menu <- menuItem(labelInput("comparacion"), tabName = "comparar", icon = icon("eye"))
  
  new.prediction.menu <- menuItem(labelInput("predicnuevos"), tabName = "predNuevos", icon = icon("table"))
  
  info.menu <- menuItem(labelInput("acercade"), tabName = "acercaDe", icon = icon("info"))
  
  menu.language <- tags$li(class = "nodisabled treeview",
                           tags$a(href = "#shiny-tab-tabdioma",
                                  tags$i(class="fa fa-language"),
                                  labelInput("idioma"),
                                  tags$i(class="fa fa-angle-left pull-right")),
                           tags$ul(class="treeview-menu", style="display: none;", `data-expanded`="Idioma",
                                   radioButtons('idioma', labelInput("selidioma"), c('Español'='es', 'English'='en')),
                                   tags$br()))
  
  #Los sliderInput y colourpicker por un motivo imprevisto se tienen que inicializar
  #De lo contrario no se van a mostrar en algunas partes de la interfaz
  init.inputs <- tags$div(style = "display:none;",
                          sliderInput(inputId = "aux", min = 2, value = 2,
                                      label = "Cantidad de Clusters", max = 10),
                          colourpicker::colourInput(
                            "auxColor", NULL, value = "red", allowTransparent = T))
  
  # The side menu
  mi.menu <- sidebarMenu(id = "principal",
                         tags$div(id = "espacioMenu"),
                         load.menu,
                         statistics.menu,
                         supervised.learning.menu,
                         compare.menu,
                         new.prediction.menu,
                         #report.menu,
                         info.menu,
                         hr(),
                         menu.language,
                         init.inputs)
  
  
  # HEAD HTML ---------------------------------------------------------------------------------------------------------------
  
  #Imports .css and .js, also decide the icon
  mi.head <- tags$head(
    tags$link(rel="icon", href="https://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
    useShinyjs())
  
  #The loading page
  load.page <- conditionalPanel(condition="($('html').hasClass('shiny-busy'))",
                                div(id = "loaderWrapper", div(id="loader")))
  
  
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      title="PROMiDAT - RegressoR",
      dashboardHeader(
        title = tags$a(href="https://promidat.com", target = "_blank",
                       img(src="img/Logo2.png", height=55, width="100%",
                           id="imgPromidat"))),
      dashboardSidebar(mi.menu),
      dashboardBody(mi.head,
                    load.page,
                    tabItems(
                      tabItem(tabName = "cargar",  mod_load_data_ui("load_data_ui_1")),
                      tabItem(tabName = "resumen",  mod_r_numerico_ui("r_numerico_ui_1")),
                      tabItem(tabName = "normalidad",  mod_normal_ui("normal_ui_1")),
                      tabItem(tabName = "dispersion",  mod_dispersion_ui("dispersion_ui_1")),
                      tabItem(tabName = "distribucion",  mod_distribuciones_ui("distribuciones_ui_1")),
                      tabItem(tabName = "correlacion",  mod_correlacion_ui("correlacion_ui_1")),
                      tabItem(tabName = "poderPred",  mod_Predictive_Power_ui("Predictive_Power_ui_1")),
                      tabItem(tabName = "rl",  mod_linear_regression_ui("linear_regression_ui_1")),
                      tabItem(tabName = "rlr",  mod_penalized_Regression_ui("penalized_Regression_ui_1")),
                      tabItem(tabName = "dt",  mod_regression_trees_ui("regression_trees_ui_1")),
                      tabItem(tabName = "comparar",  mod_model_comparison_ui("model_comparison_ui_1")),
                      tabItem(tabName = "acercaDe",  mod_information_page_ui("information_page_ui_1"))
                      ))
      )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path('www', app_sys('app/www'))
  add_resource_path('img', app_sys('app/img'))
  add_resource_path('lang', app_sys('app/lang'))
  
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'RegressoR'
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
  
}

