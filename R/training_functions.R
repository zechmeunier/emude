#' Train
#'
#' Trains the model on the training data provided in the model definition
#'
#' @param model description
#' @param loss_function description
#' @param optimizer description
#' @param regularization description
#' @param verbose description
#' @param loss_options description
#' @param optim_options description
#' @return description
#' @export
train <- function(
    model,
    loss_function = "derivative matching",
    optimizer = "ADAM",
    regularization_weight = 0.0,
    verbose = TRUE,
    loss_options = list(),
    optim_options = list())

{
  verbose <- ifelse(verbose,"true","false")
  JuliaCall::julia_assign("loss_options",loss_options)
  JuliaCall::julia_assign("optim_options",optim_options)
  JuliaCall::julia_eval(paste0("train!(",model,",loss_function=","\"", loss_function, "\"",
                    ",optimizer=","\"",optimizer,"\"",
                    ",regularization_weight=",regularization_weight,
                    ",verbose=",verbose,
                    ",loss_options=NamedTuple(loss_options)",
                    ",optim_options=NamedTuple(optim_options))"))
  print("Done! :)")
}
