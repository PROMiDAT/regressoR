
#' gg_color_hue
#' 
#' @description Create colors
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
#' @description Scatter plot between the actual value of the variable to be predicted and the prediction of the model.
#'
#' @param real the real values in traning-testing.
#' @param prediction the prediction values in traning-testing.
#' @param model the name of the model of the scatter plot.
#'
#' @return a ggplot graphic
#' @export
#'
#' @examples
#' real <- rnorm(45)
#' prediction <- rnorm(45)
#' model <- "KNN"
#' plot_real_prediction(real, prediction, model)
#' 
plot_real_prediction <- function(real, prediction, model = "") {
  ggplot(data = data.frame(real = real, prediction = as.numeric(prediction)), mapping = aes(x = real, y = prediction)) +
    geom_point(size = 2, col = "red") +
    labs(title = paste0("Real vs Prediction", ifelse(model == "", "", paste(", con", model))), x = "Real", y = "Prediction") +
    theme_minimal() +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_line(col = "black",  mapping = aes(x = real, y = real), alpha = 0.5)
}


#' error_plot
#' 
#' @description makes a warning graphic
#'
#' @param msg the message to be displayed in the graph
#'
#' @return a ggplot graphic
#' @export
#'
#' @examples
#' error_plot("My Warning")
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


#' error.variables
#'
#' @description Draws an error of missing data 
#'
#' @param num if TRUE shows a message of missing numerical variables, if FALSE shows a message of missing categorical variables.
#' @param language The language to choose. It can be "es" or "en".
#' 
#' @export
#'
#' @examples
#' error.variables(TRUE)
#' error.variables(FALSE)
#' 
error.variables <- function(num = T, language = "es") {
  if(num){
    error_plot(translate("errornum", language))
  } else {
    error_plot(translate("errorcat", language))
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
  par(mar=c(3.1, 3.1, 1.1, 2.1))
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
  ggplot(data, aes(data$label, data$value)) +
    geom_bar(stat = 'identity', fill = colores) +
    geom_text(aes(label = data$value, y = data$value), vjust = -0.5, size = 4) +
    theme_minimal() + labs(x = "label", y = "value")
}