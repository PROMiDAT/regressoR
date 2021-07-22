#' boosting_model
#' 
#' @description generates the code to create the boosting model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param n.trees the n.trees parameter of the model.
#' @param distribution the distribution parameter of the model.
#' @param shrinkage the shrinkage parameter of the model.
#'
#' @seealso \code{\link[gbm]{gbm}}
#'
#' @export
#' 
#' @examples
#' library(gbm)
#' library(dplyr)
#' 
#' x <- boosting_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.boosting)
#' 
boosting_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.boosting", n.trees = 50, distribution = "gaussian", shrinkage = 0.1){
  n.trees <- ifelse(!is.numeric(n.trees), 50, n.trees)
  shrinkage <- ifelse(!is.numeric(shrinkage), 0.1, shrinkage)
  extra.values <- calibrate_boosting(exe(data))
  
  if(is.null(extra.values)){
    codigo <- paste0(model.var,"<- gbm(`",variable.pred,
                     "`~ ., data = ",data,", distribution = '",
                     distribution,"', n.trees = ",n.trees,", shrinkage = ",shrinkage,")")
  }else{
    codigo <- paste0(model.var," <- gbm(`",variable.pred,
                     "`~ ., data = ",data,", distribution = '",
                     distribution,"', n.trees = ",n.trees,", shrinkage = ",shrinkage,",n.minobsinnode = ",extra.values[["n.minobsinnode"]],
                     ",bag.fraction = ",extra.values[["bag.fraction"]],")")
  }
  return(codigo)
}

#' boosting_prediction
#' 
#' @description generates the code to create the prediction of the boosting model.
#'
#' @param data the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#' @param n.trees the n.trees parameter of the model.
#'
#' @seealso \code{\link[gbm]{gbm}}
#' 
#' @export
#'
#' @examples
#' library(gbm)
#' library(dplyr)
#' x <- boosting_model('iris', 'Petal.Length', "model_boosting")
#' exe(x)
#' print(model_boosting)
#' 
#' x <- boosting_prediction('iris', 'Petal.Length', 'model_boosting', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
boosting_prediction <- function(data = "datos.prueba", variable.pred = NULL, model.var = "modelo.boosting", pred.var = "prediccion.boosting", n.trees = 50) {
  n.trees <- ifelse(!is.numeric(n.trees), 50, n.trees)
  return(paste0(pred.var," <- predict(",model.var,
                ", ",data," %>% select(-`",variable.pred,"`), n.trees = ",n.trees,")"))
}

#' boosting_importance_plot
#' 
#' @description generates the code to make the graph of variable importance.
#'
#' @param model.var the name of the variable that stores the resulting model.
#' @param data the name of the learning data.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' library(gbm)
#' library(ggplot2)
#' library(forcats)
#' library(dplyr)
#' 
#' x <- boosting_model('iris', 'Petal.Length', "model_boosting")
#' exe(x)
#' 
#' x <- boosting_importance_plot('model_boosting', 'iris')
#' exe(x)
#' }
# boosting_importance_plot <- function(model.var = "modelo.boosting", data = "datos.aprendizaje"){
#   data <- exe(data)
#   size.y <- ifelse(ncol(data) <= 25, 1.5, 1 - (ncol(data) - 25)/4 * 0.01 )
#   size.y <- ifelse(size.y <= 0, 0.1, size.y)
#   paste0("ggplot(summary(",model.var,"), aes(x = fct_reorder(var, rel.inf), y = rel.inf, fill = fct_reorder(var, rel.inf))) +\n",
#          "geom_bar(stat = 'identity', position = 'identity', width = 0.1) +\n",
#          "labs(title = '",translate("impVarRI"),"', y = '",translate("RI"),"', x = '') +\n",
#          "scale_y_continuous(labels = scales::comma) + coord_flip() +\n",
#          "theme(axis.text.x = element_text(angle = 45, hjust = 1),",
#          "axis.text.y=element_text(size=rel(",size.y,")),legend.position='none')\n")
# }