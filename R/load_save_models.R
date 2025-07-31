#' Load Julia Model Weights
#'
#' Description here
#'
#' @param model description
#' @param file description
#' @return description
#' @export
load_julia_model_weights <- function(model,file){
  JuliaCall::julia_eval(paste0("load_model_parameters!(",model,",",file,")"))
}

save_julia_model_weights <- function(model,file){
  JuliaCall::julia_eval(paste0("save_model_parameters(",model,",",file,")"))
}
