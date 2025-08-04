#' Load the Julia model weights
#'
#' Loads the weights and other parameters of the process model from Julia
#'
#' @param model User-defined model containing the weights, biases, and other parameters.
#' @param file File path from which to read the model parameters. Files are saved in JSON format.
#'
#' @return A data frame containing the parameters from the trained model.
#' @export
#'
#' @examples
#' print(x)
#'
load_julia_model_weights <- function(model,file){
  JuliaCall::julia_eval(paste0("load_model_parameters!(",model,",",file,")"))
}

#' Save the Julia model weights
#'
#' @param model User-defined model containing the weights, biases, and other parameters.
#' @param file File path to which to write the model parameters. Files are saved in JSON format.
#'
#' @return A file containing the parameters from the trained model.
#' @export
#'
#' @examples
#' print(x)
#'
save_julia_model_weights <- function(model,file){
  JuliaCall::julia_eval(paste0("save_model_parameters(",model,",",file,")"))
}
