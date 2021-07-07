
#' gg_color_hue
#' 
#' @description create colors.
#'
#' @param n an integer specifying the number of colors to create.
#'
#' @return color-coded vector
#' @export
#'
#' @examples
#' col <- gg_color_hue(3)
#' plot(iris$Species, col = col)
#' 
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' plot_real_prediction
#'
#' @description scatter plot between the actual value of the variable to be predicted and the prediction of the model.
#'
#' @param real the real values in traning-testing.
#' @param prediction the prediction values in traning-testing.
#' @param model the name of the model of the scatter plot.
#' @param titles Labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @export
#'
#' @examples
#' real <- rnorm(45)
#' prediction <- rnorm(45)
#' model <- "KNN"
#' plot_real_prediction(real, prediction, model)
#' 
plot_real_prediction <- function(real, prediction, model = "", titles = c("Predicciones vs Valores Reales",
                                                                          "Valor Real","Predicción")) {

  #Coordenadas para los puntos
  prediction <- unname(prediction)
  x_y.values <- list()
  for (i in 1:dim(real)[1]) {
    x_y.values[[i]] <- list(name = as.character(rownames(real)[i]),value = c(real[i,1],prediction[i]))
  }

  #Coordenadas para la linea
  line.Values <- list()
  minimo <- floor(min(real))
  maximo <- ceiling(max(real))
  values <- minimo:maximo
  for (i in 1:length(values)) {
    line.Values[[i]] <- list(value = c(values[i],values[i]))
  }

  opts <- list(
    xAxis = list(
      type = "value"
    ),
    yAxis = list(
      type = "value"
    ),
    series = list(
      list(
        type = "scatter",
        symbolSize = 10,
        color = "red",
        data = x_y.values,
        tooltip = list(formatter = htmlwidgets::JS(paste0(
        "function(params){
        return(params.marker + '<br/><b> ID: </b>' + params.name + '<br /><b>",
        titles[2],": </b>' + params.value[0].toFixed(4) + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(4))
      }
    ")))),
      list(
        type = "line",
        symbol = "none",
        lineStyle = list(width = 2),
        tooltip = list(show = F),
        color = "black",
        data = line.Values
      )
    )
  )

  e_charts() %>%
    e_list(opts) %>%
    e_title(text = paste0(titles[1],"  (",model,")")) %>%
    e_axis_labels(x = titles[2], y = titles[3]) %>%
    e_tooltip() %>%
    e_datazoom(show = F) %>%
    e_show_loading()
}


#' error_plot
#' 
#' @description makes a warning graphic
#'
#' @param msg the message to be displayed in the graph
#'
#' @export
#'
#' @examples
#' error_plot("My Warning")
#' 
error_plot <- function(msg) {
  x <- c(2, 2.5, 3)
  y <- c(2 ,3 ,2)
  res <- ggplot(data.frame(x = x, y = y)) +
    geom_polygon(mapping = aes(x = x, y = y), col="gold", fill="gold", alpha=0.3) +
    annotate("rect", xmin = 2.47, xmax = 2.53, ymin = 2.4, ymax = 2.8) +
    annotate("rect", xmin = 2.47, xmax = 2.53, ymin = 2.25, ymax = 2.35) +
    annotate("text", x = 2.5, y = 2.1, label = paste0("bold('", msg, "')"),
             size = 8, parse = T) +
    theme(
      panel.background = element_rect(fill = "transparent"),
      axis.title = element_blank(), axis.ticks = element_blank(),
      axis.text = element_blank()
    )
  return(res)
}


#' error_variables
#'
#' @description draws an error of missing data.
#'
#' @param num if TRUE shows a message of missing numerical variables, if FALSE shows a message of missing categorical variables.
#' 
#' @export
#'
#' @examples
#' error_variables(TRUE)
#' error_variables(FALSE)
#' 
error_variables <- function(num = T) {
  if(num){
    error_plot(translate("errornum"))
  } else {
    error_plot(translate("errorcat"))
  }
}

#' numerical_distribution
#'
#' @description makes the graph of the numerical distribution.
#'
#' @param var a vector with the data for the numerical distribution chart. 
#' @param var.name the name of the variable. 
#' @param color the color of the chart.
#'
#' @export
#'
#' @examples
#' numerical_distribution(iris[,'Sepal.Length'], 'Sepal.Length', 'red')
#' 
numerical_distribution <- function(var, var.name, color){
  nf <- graphics::layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE),  height = c(3,1))
  opar <- par(mar=c(3.1, 3.1, 1.1, 2.1))
  on.exit({
    par(opar)
    nf <- graphics::layout(mat = matrix(c(1, 1), 2, 1, byrow=TRUE),  height = c(3,1))
  })
  hist(var, col = color, border=F, axes=F, main = var.name)
  axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  boxplot(var, col = color, boxcol = color, boxlty = 1, boxlwd = 3,
          boxwex = 1.5, edcol = color, medlty = 1, medlwd = 8, axes=F,
          medcol = color, whiskcol = color, whisklty = 3, staplecol = color,
          staplelty = 1, staplelwd = 3, horizontal = TRUE, outline=TRUE,
          frame=F, whisklwd = 2.5, outpch = 20, outcex = 1.5, outcol = 'red')
}

#' categorical_distribution
#'
#' @description makes the graph of the categorical distribution.
#'
#' @param var a vector with the data for the categorical distribution chart. 
#'
#' @export
#'
#' @examples
#' categorical_distribution(iris$Species)
#' 
categorical_distribution <- function(var) {
  colores <- sapply(levels(var),function(i) rgb(runif(1), runif(1), runif(1), 0.8))
  data <- data.frame(label = levels(var), value = summary(var))
  ggplot(data, aes(label, value)) +
    geom_bar(stat = 'identity', fill = colores) +
    geom_text(aes(label = value, y = value), vjust = -0.5, size = 4) +
    theme_minimal() + ggplot2::labs(x = "label", y = "value")
}



#' e_posib_lambda
#' 
#' @description Graph a cv.glmnet model
#'
#' @param cv.glm a cv.glmnet model.
#' @param log.lambda number that specifies the logarithm of the selected lambda
#' @param titles labels on the chart
#'
#' @seealso \code{\link[glmnet]{cv.glmnet}}
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' 
#' @export
#' 
e_posib_lambda <- function(cv.glm, log.lambda = NULL, titles = c("Error Cuadrático Medio","Curva Inferior",
                                                                 "Curva Superior","Seleccionado",
                                                                 "Coeficientes Distintos de Cero")){
  x  <- log(cv.glm$lambda)
  y  <- cv.glm$cvm
  x1 <- x[cv.glm$index[[1]]]
  x2 <- x[cv.glm$index[[2]]]
  upper <- cv.glm$cvup
  lower <- cv.glm$cvlo
  nzero  <- cv.glm$nzero
  data.lambda <- data.frame(x, y, upper, lower, nzero)

  grafico  <- data.lambda %>%
    e_charts(x) %>%
    e_scatter(y, symbol_size = 11, color = "red", 
              tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                "return(params.marker + '<br/>' + ",
                                                                "'<b>Log(lambda): </b>' + ",
                                                                "Number.parseFloat(params.value[0]).toFixed(6) + ",
                                                                "'<br/><b>", titles[1], ": </b>' + ",
                                                                "Number.parseFloat(params.value[1]).toFixed(6))}")))) %>%
    e_error_bar(lower, upper,
                tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                  "return('<b>", titles[2], ": </b>' + ",
                                                                  "Number.parseFloat(params.value[1]).toFixed(6) + ",
                                                                  "'<br/><b>", titles[3], ": </b>' + ",
                                                                  "Number.parseFloat(params.value[2]).toFixed(6))}")))) %>%
    e_mark_line(title = "Log(lambda.min)", 
                data = list(xAxis = x1, 
                            tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                              "return('<b>Log(lambda.min): </b>' + ",
                                                                              "Number.parseFloat(params.value).toFixed(6))}"))))) %>%
    e_mark_line(title = "Log(lambda.1se)", 
                data = list(xAxis = x2,
                            tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                              "return('<b>Log(lambda.1se): </b>' + ",
                                                                              "Number.parseFloat(params.value).toFixed(6))}")))))
  
  #Si se eligió manualmente un lambda
  if(!is.null(log.lambda)){
    grafico <- grafico %>% 
      e_mark_line(title = titles[4], 
                  data = list(xAxis = log.lambda,
                              lineStyle = list(color = "blue"),
                              tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                                "return('<b>Log(lambda) ",titles[4], ": </b>' + ",
                                                                                "Number.parseFloat(params.value).toFixed(6))}")))))
  }
  
  # number of non-zero coefficients at each lambda
  grafico <- grafico %>%
    e_line(nzero, x_index = 1, y_index = 1, tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                                              "return('<b>Log(lambda): </b>' + ",
                                                                                              "Number.parseFloat(params.value[0]).toFixed(6) + ",
                                                                                              "'<br/><b>", titles[5],": </b>' + ",
                                                                                              "params.value[1])}")))) %>%
    e_grid(height = "40%") %>%
    e_grid(height = "30%", top = "65%") %>%
    e_x_axis(type = 'value', minInterval = 1, min = floor(min(data.lambda$x)), gridIndex = 0, index = 0, name = "Log(lambda)") %>%
    e_y_axis(type = 'value', axisLine = list(onZero = F), gridIndex = 0, index = 0, name = titles[1]) %>%
    e_x_axis(type = 'value', minInterval = 1, min = floor(min(data.lambda$x)), gridIndex = 1, index = 1, name = "Log(lambda)") %>%
    e_y_axis(type = 'value', axisLine = list(onZero = F), gridIndex = 1, index = 1, axisLine = list(onZero = F), name = titles[5]) %>%
    e_legend(FALSE) %>% 
    e_tooltip(trigger = "item") %>% e_datazoom(show = F) %>% e_show_loading()

  
  return(grafico)
}


#' e_coeff_landa
#' 
#' @description Graph the coefficients and lambdas of a cv.glmnet model
#'
#' @param cv.glm a cv.glmnet model.
#' @param log.lambda number that specifies the logarithm of the selected lambda
#' @param titles labels on the chart
#'
#' @seealso \code{\link[glmnet]{cv.glmnet}}
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' 
#' @export
#' 
e_coeff_landa <- function(cv.glm, log.lambda = NULL, titles = c("Coeficientes","Seleccionado")){
  
  data   <- data.frame(t(as.data.frame(as.matrix(cv.glm$glmnet.fit$beta))))
  x      <- log(cv.glm$glmnet.fit$lambda)
  data   <- cbind(x = x, data)
  data   <- data[order(data$x),]
  #lambda <- ifelse(best.lambda %in% data$x, best.lambda, log(cv.glm$lambda.min))
  new    <- data.frame()
  for (nom in colnames(data)[-1]) {
    x      <- data[["x"]]
    y      <- data[[nom]]
    nombre <- nom
    new.   <- data.frame(x = x, y = y, nombre = nombre)
    new    <- rbind(new, new.)
  }
  
  
  grafico <- new %>%
    group_by(nombre) %>%
    e_charts(x) %>%
    e_line(y, bind = nombre, 
           tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                             "return(params.marker + ",
                                                             "'<b>' + params.seriesName + '</b><br/>' + ",
                                                             "'<b>Log(lambda.min): </b>' + ",
                                                             "Number.parseFloat(params.value[0].toFixed(6)) + '<br/>' + ",
                                                             "'<b>", titles[1], ": </b>' + ",
                                                             "Number.parseFloat(params.value[1].toFixed(6)))}")))) %>%
    e_mark_line(title = "Log(lambda.min)", 
                data = list(xAxis = log(cv.glm$lambda.min), lineStyle = list(color = 'black'), 
                            tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                              "return('<b>Log(lambda.min): </b>' + ",
                                                                              "Number.parseFloat(params.value).toFixed(6))}"))))) %>%
    e_x_axis(name = "Log(lambda)", axisLine = list(onZero = F)) %>%
    e_y_axis(name = titles[1],axisLine = list(onZero = F)) %>%
    e_labels(position = 'left',formatter = htmlwidgets::JS("
                                        function(params){
                                        if(params.dataIndex==0){
                                        return(params.name)
                                        }else
                                        {return('')}}"))%>%
    e_legend(show = FALSE) %>% e_tooltip() %>% 
    e_datazoom(show = F) %>% e_show_loading()
  
  #Si se eligió manualmente un lambda
  if(!is.null(log.lambda)){
    grafico <- grafico %>% 
      e_mark_line(title = titles[2], 
                  data = list(xAxis = log.lambda,
                              lineStyle = list(color = "blue"),
                              tooltip = list(formatter = htmlwidgets::JS(paste0("function(params){",
                                                                                "return('<b>Log(lambda) ", titles[2],": </b>' + ",
                                                                                "Number.parseFloat(params.value).toFixed(6))}")))))
  }
  
  return(grafico)
}


#' importance_plot_rf
#' 
#' @description graphs the importance of variables for the random forest model according to the percentage increase in mean square error.
#'
#' @param model.rf a random forest model.
#' @param titles labels on the chart
#'
#' @seealso \code{\link[randomForest]{randomForest}}
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' 
#' @export
#'
#'
#' @examples
#' library(randomForest)
#' x <- rf_model('iris', 'Petal.Length')
#' exe(x)
#' importance_plot_rf(modelo.rf)
#' 
importance_plot_rf <- function(model.rf, titles = c("Importancia de Variables Según el Porcentaje de Incremento del MSE",
                               "Aumento porcentual del error cuadrático medio", "Variable")){
  #https://www.displayr.com/how-is-variable-importance-calculated-for-a-random-forest/
  df <- as.data.frame(model.rf$importance)
  df$variables <- as.factor(rownames(df))
  df <- df[order(df$`%IncMSE`, decreasing = T),]
  
  e_charts(data = df, x = variables) %>%
    e_bar(serie = `%IncMSE`,legend = NULL) %>%
    echarts4r::e_flip_coords() %>%
    e_title(text = titles[1]) %>%
    e_x_axis(name = titles[2], nameLocation = "center", 
             nameTextStyle = list(padding = c(10,0,0,0)),
             interval = 10,
             axisLabel = list(formatter = '{value} %')) %>%
    e_y_axis(name = titles[3], nameLocation = "start", inverse = T) %>%
    e_tooltip(formatter = htmlwidgets::JS("function(params){
    console.log(params)
    return('<b>' +  params.value[1] + ': </b>' + Number.parseFloat(params.value[0]).toFixed(4) + '%')
    }
    ")) %>%
    e_datazoom(show = F) %>%
    e_show_loading()
}


boosting_importance_plot <- function(model, titles = c("Importancia de Variables según Influencia Relativa",
                                                       "Influencia Relativa","Variable")){
  df <- summary.gbm(model,order = T, plotit = F)

  e_charts(data = df, x = var) %>%
    e_bar(serie = rel.inf,legend = NULL) %>%
    echarts4r::e_flip_coords() %>%
    e_title(text = titles[1]) %>%
    e_x_axis(name = titles[2], nameLocation = "center", 
             nameTextStyle = list(padding = c(10,0,0,0)),
             interval = 10,
             axisLabel = list(formatter = '{value} %')) %>%
    e_y_axis(name = titles[3], nameLocation = "start", inverse = T) %>%
    e_tooltip(formatter = htmlwidgets::JS("function(params){
    console.log(params)
    return('<b>' +  params.value[1] + ': </b>' + Number.parseFloat(params.value[0]).toFixed(4) + '%')
    }
    ")) %>%
    e_datazoom(show = F) %>%
    e_show_loading()
}



#' plot_RMSE
#' 
#' @description graph the root mean square error of cross validation according to components used.
#'
#' @param model a dimension reduction model.
#' @param n.comp the optimum number of components.
#' @param titles labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' 
#' @export
#'
#' @examples
#' library(pls)
#' 
#' x <- rd_model('iris', 'Petal.Length')
#' exe(x)
#' 
#' plot_RMSE(modelo.rd,1)
#' 
plot_RMSE <- function(model, n.comp, titles = c("RMSE Según Número de Componentes",
                                                "Número de Componente","RMSE")){
  
  RMSE.CV <- pls::RMSEP(model)$val[1, 1, ]
  df <- data.frame(Componentes = 0:(length(RMSE.CV) - 1), Error = RMSE.CV)
  
  #Coordenadas para los puntos
  x_y.RMSE <- list()
  for (i in 1:dim(df)[1]) {
    x_y.RMSE[[i]] <- list(value = c(df[i,1],df[i,2]))
  }
  
  #Coordenadas para la linea
  line.Values <- list()
  maximo <- ceiling(max(df[,2]))
  values <- 0:maximo
  for (i in 1:length(values)) {
    line.Values[[i]] <- list(value = c(n.comp,values[i]))
  }
  
  opts <- list(
    xAxis = list(
      type = "value",
      name = titles[2],
      nameTextStyle = list(fontSize = 13),
      max = max(df[,1]),
      interval = 2
    ),
    yAxis = list(
      type = "value",
      name = titles[3],
      nameTextStyle = list(fontSize = 13),
      max = maximo
    ),
    series = list(
      list(
        type = "line",
        symbolSize = 6,
        lineStyle = list(width = 2,type = 'solid'),
        color = "#4682B4",
        data = x_y.RMSE,
        tooltip = list(formatter = htmlwidgets::JS(paste0(
          "function(params){
          return('<b>",titles[2],": </b>' + params.value[0] + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(4))
      }
    ")))),
      list(
        type = "line",
        symbol = "none",
        lineStyle = list(width = 2, type = 'dashed'),
        tooltip = list(show = F),
        color = "blue",
        data = line.Values
      )
    )
  )
  
  e_charts() %>%
    e_list(opts) %>%
    e_title(text = titles[1]) %>%
    e_tooltip() %>%
    e_datazoom(show = F) %>%
    e_show_loading()
}

#' plot_pred_rd
#' 
#' @description graph of variance explained in the predictors according to components used.
#'
#' @param model a dimension reduction model.
#' @param n.comp the optimum number of components.
#' @param titles labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @export
#'
#' @examples
#' library(pls)
#' 
#' x <- rd_model('iris', 'Petal.Length')
#' exe(x)
#' 
#' plot_pred_rd(modelo.rd,1)
#' 
plot_pred_rd <- function(model, n.comp, titles = c("Varianza Explicada en Predictores",
                                                   "Número de Componentes","Porcentaje de Varianza Explicada")){

  
  var.explicada <- cumsum(pls::explvar(model)) / 100
  df <- data.frame(Componentes = 1:length(var.explicada), Varianza = var.explicada * 100)
  
  # Coordenadas x,y
  x_y.Varianza <- list()
  for (i in 1:dim(df)[1]) {
    x_y.Varianza[[i]] <- list(value = c(df[i,1],df[i,2]))
  }
  
  #Coordenadas para la linea
  line.Values <- list()
  maximo <- ceiling(max(df[,2]))
  values <- 0:maximo
  for (i in 1:length(values)) {
    line.Values[[i]] <- list(value = c(n.comp,values[i]))
  }
  
  opts <- list(
    xAxis = list(
      type = "value",
      name = titles[2],
      nameTextStyle = list(fontSize = 13),
      max = max(df[,1]),
      interval = 2
    ),
    yAxis = list(
      type = "value",
      name = titles[3],
      nameTextStyle = list(fontSize = 13),
      axisLabel = list(formatter = '{value} %'),
      max = maximo
    ),
    series = list(
      list(
        type = "line",
        symbolSize = 6,
        lineStyle = list(width = 2,type = 'solid'),
        color = "#4682B4",
        data = x_y.Varianza,
        tooltip = list(formatter = htmlwidgets::JS(paste0(
          "function(params){
          return('<b>",titles[2],": </b>' + params.value[0] + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(4))
      }
    ")))),
      list(
        type = "line",
        symbol = "none",
        lineStyle = list(width = 2, type = 'dashed'),
        tooltip = list(show = F),
        color = "blue",
        data = line.Values
      )
    )
  )
  
  e_charts() %>%
    e_list(opts) %>%
    e_title(text = titles[1]) %>%
    e_tooltip() %>%
    e_datazoom(show = F) %>%
    e_show_loading()
}

#' plot_var_pred_rd
#' 
#' @description graph of the variance explained in the variable to predict according to the components used.
#'
#' @param model a dimension reduction model.
#' @param n.comp the optimum number of components.
#' @param titles labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @export
#'
#' @examples
#' library(pls)
#' 
#' x <- rd_model('iris', 'Petal.Length')
#' exe(x)
#' 
#' plot_var_pred_rd(modelo.rd,1)
#' 
plot_var_pred_rd <- function(model, n.comp, titles = c("Varianza Explicada en Variable a Predecir",
                                                       "Número de Componente","Porcentaje de Varianza Explicada")){
  
  var.explicada <- drop(pls::R2(model, estimate = "train", intercept = FALSE)$val)
  df <- data.frame(Componentes = 1:length(var.explicada), Varianza = var.explicada * 100)
  
  # Coordenadas x,y
  x_y.Varianza <- list()
  for (i in 1:dim(df)[1]) {
    x_y.Varianza[[i]] <- list(value = c(df[i,1],df[i,2]))
  }
  
  #Coordenadas para la linea
  line.Values <- list()
  maximo <- ceiling(max(df[,2]))
  values <- 0:maximo
  for (i in 1:length(values)) {
    line.Values[[i]] <- list(value = c(n.comp,values[i]))
  }
  
  opts <- list(
    xAxis = list(
      type = "value",
      name = titles[2],
      nameTextStyle = list(fontSize = 13),
      max = max(df[,1]),
      interval = 2
    ),
    yAxis = list(
      type = "value",
      name = titles[3],
      nameTextStyle = list(fontSize = 13),
      axisLabel = list(formatter = '{value} %'),
      max = maximo
    ),
    series = list(
      list(
        type = "line",
        symbolSize = 6,
        lineStyle = list(width = 2,type = 'solid'),
        color = "#4682B4",
        data = x_y.Varianza,
        tooltip = list(formatter = htmlwidgets::JS(paste0(
          "function(params){
          return('<b>",titles[2],": </b>' + params.value[0] + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(4))
      }
    ")))),
      list(
        type = "line",
        symbol = "none",
        lineStyle = list(width = 2, type = 'dashed'),
        tooltip = list(show = F),
        color = "blue",
        data = line.Values
      )
    )
  )
  
  e_charts() %>%
    e_list(opts) %>%
    e_title(text = titles[1]) %>%
    e_tooltip() %>%
    e_datazoom(show = F) %>%
    e_show_loading()
}