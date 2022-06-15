# KNN PAGE ----------------------------------------------------------------------------------------------------------------
#' 
#' kkn_model <- function(data, variable.pred, scale = TRUE, k = 7, kernel = "optimal", distance = 2){
#'   #Revisar error de library/import
#'   #library("kknn")
#'   eval(parse(text = "library('kknn')"))
#'   if(!is.null(variable.pred) && !is.null(data)){
#'     form <- formula(paste0(variable.pred,"~."))
#'     modelo.knn <- kknn::train.kknn(form, data = data, scale = scale, ks = k, kernel = kernel, distance = distance)
#'     #Cambiamos la forma en que va aparecer el call
#'     modelo.knn$call$formula <- form
#'     modelo.knn$call$ks <- k
#'     modelo.knn$call$kernel <- kernel
#'     modelo.knn$call$scale <- scale
#'     modelo.knn$call$distance <- distance
#'     return(modelo.knn)
#'   }
#'   return(NULL)
#'   # kmax <- ifelse(!is.numeric(kmax), exe("round(sqrt(nrow(",data,"))"), kmax)
#'   # return(paste0(model.var," <- train.kknn(",variable.pred,"~., data = ",data,", scale =",scale,", kmax=",kmax,", kernel = '",kernel,"', distance = ",distance,")"))
#' }


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


#------------------------------------CODE---------------------------------------
codeKnn <- function(variable.predecir, scale, k, kernel, distance){
  return(paste0("kkn_model(data, '",variable.predecir,"', scale = ",scale, ", k = ", k,
                ", kernel = '",kernel,"', distance = ", distance, ")\n"))
}

codeKnnPred <- function(nombreModelo = "knn.model"){
  return(paste0("kkn_prediction(model = ", nombreModelo, ", test.data)\n"))
}

codeKnnIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.knn)\n"))
}