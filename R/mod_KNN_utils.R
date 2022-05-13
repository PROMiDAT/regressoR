# KNN PAGE ----------------------------------------------------------------------------------------------------------------

#' kkn_model
#' 
#' @description generates a k nearest neighbors model.
#' 
#' @param data dataframe
#' @param variable.pred the name of the variable to be predicted.
#' @param scale the scale parameter of the model.
#' @param k the k value of the model.
#' @param kernel string. The kernel parameter of the model.
#' @param distance the distance parameter of the model.
#' 
#' @seealso \code{\link[kknn]{train.kknn}}
#' 
#' @export
#' 
kkn_model <- function(data, variable.pred, scale = TRUE, k = 7, kernel = "rectangular", distance = 2){
  #Revisar error de library/import
  #library("kknn")
  eval(parse(text = "library('kknn')"))
  if(!is.null(variable.pred) && !is.null(data)){
    form <- formula(paste0(variable.pred,"~."))
    modelo.knn <- kknn::train.kknn(form, data = data, scale = scale, ks = k, kernel = kernel, distance = distance)
    #Cambiamos la forma en que va aparecer el call
    modelo.knn$call$formula <- form
    modelo.knn$call$ks      <- k
    modelo.knn$call$kernel <- kernel
    modelo.knn$call$scale  <- scale
    modelo.knn$call$distance <- distance
    modelo <<-  modelo.knn
    
    return(modelo.knn)
  }
  return(NULL)
  # kmax <- ifelse(!is.numeric(kmax), exe("round(sqrt(nrow(",data,"))"), kmax)
  # return(paste0(model.var," <- train.kknn(`",variable.pred,"`~., data = ",data,", scale =",scale,", kmax=",kmax,", kernel = '",kernel,"', distance = ",distance,")"))
}


#------------Tomadas del paquete kknn por error sin library---------------------
# contr.dummy <- function (n, contrasts = TRUE) 
# {
#   if (length(n) <= 1) {
#     if (is.numeric(n) && length(n) == 1 && n > 1) 
#       levels <- 1:n
#     else stop("contrasts are not defined for 0 degrees of freedom")
#   }
#   else levels <- n
#   lenglev <- length(levels)
#   cont <- array(0, c(lenglev, lenglev), list(levels, levels))
#   cont[col(cont) == row(cont)] <- 1
#   cont
# }
# 
# contr.ordinal <- function (n, contrasts = TRUE) 
# {
#   if (length(n) <= 1) {
#     if (is.numeric(n) && length(n) == 1 && n > 1) 
#       levels <- 1:n
#     else stop("contrasts are not defined for 0 degrees of freedom")
#   }
#   else levels <- n
#   lenglev <- length(levels)
#   cont <- array(0.5, c(lenglev, lenglev - 1), list(levels, 
#                                                    NULL))
#   cont[lower.tri(cont)] <- -0.5
#   cont
# }
# 
# 
# contr.metric <- function (n, contrasts = TRUE) 
# {
#   if (length(n) <= 1) {
#     if (is.numeric(n) && length(n) == 1 && n > 1) 
#       levels <- 1:n
#     else stop("contrasts are not defined for 0 degrees of freedom")
#   }
#   else levels <- n
#   lenglev <- length(levels)
#   cont <- array((1:lenglev) - (1 + lenglev)/2, c(lenglev, 1), 
#                 list(levels, NULL))
#   cont
# }
#-------------------------------------------------------------------------------



#' kkn_prediction
#'
#' @description generates the prediction of the k nearest neighbors model.
#'
#' @param model k nearest neighbors model(train.kknn).
#' @param test.data dataframe.
#'
#' @export
#' 
kkn_prediction <- function(model, test.data) {
  if(!is.null(test.data) && !is.null(model)){
    return(predict(model,test.data))
  }
  return(NULL)
  #return(paste0(pred.var," <- predict(",model.var,", ",data," %>% select(-`",variable.pred,"`))"))
}

make_knn_pred = function(train, test, variable.pred, k = 7,  scale = TRUE, kernel = "rectangular", distance = 2) {
  eval(parse(text = "library('kknn')"))
  if(!is.null(variable.pred) && !is.null(train)){
    form  <- formula(paste0(variable.pred,"~."))
    modelo.knn <- kknn::train.kknn(form, data = train, scale = scale, ks = k, kernel = kernel, distance = distance)
    prediccion.knn <- kkn_prediction(modelo.knn, test)
    return(rmse(test[,variable.pred], prediccion.knn))
  }
  return(NULL)
}

rmse_k_values <- function(train, test, variable.pred, k = c(1:20),  scale = TRUE, kernel = "rectangular", distance = 2) {
  knn_rmse = sapply(k, make_knn_pred, 
                    train = train, 
                    test = test, variable.pred = variable.pred, scale = scale, kernel = kernel, distance = distance)
  best_k = k[which.min(knn_rmse)]
  
  # find overfitting, underfitting, and "best"" k
  fit_status = ifelse(k < best_k, "Over", ifelse(k == best_k, "Best", "Under"))
  knn_results = data.frame(
    k,
    round(knn_rmse, 2),
    fit_status
  )
  colnames(knn_results) = c("k", "RMSE", "Fit?")
  return(knn_results)
}
plot_RMSEK <- function(datos , modelo.knn = NULL, titles = c("RMSE Segun Numero de Vecinos",
                                                "Numero de Vecinos","RMSE")){
  modelo <- modelo.knn
  # df <- data.frame(k = 1:dim(modelo.knn$MEAN.SQU)[1],rmse = sqrt(modelo.knn$MEAN.SQU))
  # best_k <- modelo.knn$best.parameters$k
  # best_rmse <- df[best_k,2]
  df = datos
  best_k <- df[which(df$`Fit?` == "Best"),1]
  best_rmse <- df[which(df$`Fit?` == "Best"),2]
  #Coordenadas para los puntos
  x_y.RMSE <- list()
  for (i in 1:dim(df)[1]) {
    x_y.RMSE[[i]] <- list(value = c(df[i,1],df[i,2]))
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
      nameTextStyle = list(fontSize = 13)
    ),
    series = list(
      list(
        type = "line",
        symbolSize = 6,
        lineStyle = list(width = 2,type = 'solid'),
        color = "#4682B4",
        data = x_y.RMSE,
        tooltip = list(formatter = e_JS(paste0(
          "function(params){
          return('<b>",titles[2],": </b>' + params.value[0] + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(8))
      }
    "))))
    )
  )
  
  e_charts() |>
    e_list(opts) |>
    e_title(text = titles[1]) |>
    e_tooltip() |>
    e_datazoom(show = F) |>
    e_show_loading()|> 
    e_mark_line(data = list(xAxis = best_k,name = best_rmse,
                            tooltip = list(formatter = e_JS(paste0("function(params){",
                                                                   "return('<b>K Value: </b>' + ",
                                                                   "Number.parseFloat(params.value) + ",
                                                                   "'</br><b>RMSE Value: </b>' + Number.parseFloat(params.name).toFixed(8))}")))))
}

#------------------------------------CODE---------------------------------------
codeKnn <- function(variable.predecir, scale, k, kernel, distance){
  return(paste0("kkn_model(data, '",variable.predecir,"', scale = ",scale, ", k = ", k,
                ", kernel = '",kernel,"', distance = ", distance, ")"))
}

codeKnnPred <- function(nombreModelo = "knn.model"){
  return(paste0("kkn_prediction(model = ", nombreModelo, ", test.data)"))
}

codeKnnIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.knn)"))
}