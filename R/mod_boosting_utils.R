#' boosting_model

boosting_model <- function(data, variable.pred, n.trees = 50, distribution = "gaussian", shrinkage = 0.1){
  if(!is.null(variable.pred) && !is.null(data)){
    extra.values <- calibrate_boosting(data)
    form <- formula(paste0(variable.pred,"~."))
    
    if(is.null(extra.values)){
      modelo.boost <- train.gbm(form, data = data, distribution = distribution, n.trees = n.trees, shrinkage = shrinkage)
    }else{
      modelo.boost <- train.gbm(form, data = data, distribution = distribution, n.trees = n.trees, shrinkage = shrinkage,
                          n.minobsinnode = extra.values[["n.minobsinnode"]], bag.fraction = extra.values[["bag.fraction"]])
      modelo.boost$call$n.minobsinnode <- extra.values[["n.minobsinnode"]]
      modelo.boost$call$bag.fraction <- extra.values[["bag.fraction"]]
    }
    modelo.boost$call$formula <- form
    modelo.boost$call$distribution <- distribution
    modelo.boost$call$n.trees <- n.trees
    modelo.boost$call$shrinkage <- shrinkage
    return(modelo.boost)
  }
  else{return(NULL)}
}

#' boosting_prediction
#' 
#' @description generates the prediction of a boosting model.
#'
#' @param model boosting model(gbm).
#' @param test.data dataframe.
#' @param n.trees number of trees used in the prediction.
#'
#' @seealso \code{\link[gbm]{gbm}}
#' 
#' @export
#' 
boosting_prediction <- function(model, test.data, n.trees = 50) {
  if(!is.null(test.data) && !is.null(model)){
    return(predict(model,test.data, n.trees = n.trees)$prediction)
  }
  return(NULL)
  # return(paste0(pred.var," <- predict(",model.var,
  #               ", ",data," %>% select(-`",variable.pred,"`), n.trees = ",n.trees,")"))
}


#' boosting_importance_plot
#' 
#' @description generates the graph of variable importance.
#'
#' @param model boosting model(gbm).
#' @param titles Labels on the chart
#' 
#' @export
#' 
boosting_importance_plot <- function(model, titles = c("Importancia de Variables segun Influencia Relativa",
                                                       "Influencia Relativa","Variable")){
  df <- summary.gbm(model,order = T, plotit = F)
  
  e_charts(data = df, x = var) |>
    e_bar_(serie = "rel.inf" ,legend = NULL) |>
    echarts4r::e_flip_coords() |>
    e_title(text = titles[1]) |>
    e_x_axis(name = titles[2], nameLocation = "center", 
             nameTextStyle = list(padding = c(10,0,0,0)),
             interval = 10,
             axisLabel = list(formatter = '{value} %')) |>
    e_y_axis(name = titles[3], nameLocation = "start", inverse = T) |>
    e_tooltip(formatter = e_JS("function(params){
    console.log(params)
    return('<b>' +  params.value[1] + ': </b>' + Number.parseFloat(params.value[0]).toFixed(4) + '%')
    }
    ")) |>
    e_datazoom(show = F) |>
    e_show_loading()
}


#------------------------------------CODE---------------------------------------
codeBoost <- function(variable.predecir, n.trees, distribution, shrinkage){
  return(paste0("boosting_model(data, '",variable.predecir,"', n.trees = ",n.trees, ", distribution = '", distribution, "', shrinkage = ",shrinkage, ")\n"))
}

codeBoostPred <- function(nombreModelo, n.trees){
  return(paste0("boosting_prediction(model = ", nombreModelo, ", test.data, n.trees = ", n.trees, ")\n"))
}

codeBoostIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.boost)\n"))
}