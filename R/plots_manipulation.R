
#' Create colors
#'
#' @param n an integer specifying the number of colors to create.
#'
#' @return color-coded vector
#' @export
#'
#' @examples
#' col <- gg_color_hue(3)
#' plot(iris$Species, col = col)
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' Scatter plot
#'
#' Scatter plot between the actual value of the variable to be predicted and the prediction of the model.
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
#' plot.real.prediction(real, prediction, model)
plot.real.prediction <- function(real, prediction, model = "") {
  ggplot(data = data.frame(Real = real, Prediccion = as.numeric(prediction)), mapping = aes(x = Real, y = Prediccion)) +
    geom_point(size = 2, col = "red") +
    labs(title = paste0("Real vs Predicción", ifelse(model == "", "", paste(", con", model))), x = "Real", y = "Predicción") +
    theme_minimal() +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_line(col = "black",  mapping = aes(x = Real, y = Real), alpha = 0.5)
}
