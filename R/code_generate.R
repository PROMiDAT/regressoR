# MODELS UTILITY FUNCTIONS ------------------------------------------------------------------------------------------------

#' disp_models
#' 
#' @description this function generates the call code of the scatter function.
#'
#' @param prediction the name of the prediction object.
#' @param model_name the name of the model.
#' @param var_pred the name of the variable to be predicted.
#'
#' @export
#'
#' @examples
#' disp_models("prediction.knn", "KNN", "Species")
#' 
disp_models <- function(prediction, model_name, var_pred){
  
  paste0("plot_real_prediction(test.data['",var_pred,"'], ", prediction,", '",model_name,"')")
}