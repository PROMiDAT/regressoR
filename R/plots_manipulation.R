
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
