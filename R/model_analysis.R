#' Cross Validation
#'
#' description
#'
#' @param model description
#' @param k description
#' @param loss_function description
#' @param optimizer description
#' @param regularization_weight description
#' @param verbose description
#' @param loss_options description
#' @param optim_options description
#' @param path description
cross_validation <- function(
    model,
    k = 10,
    loss_function = "derivative matching",
    optimizer = "ADAM",
    regularization_weight = 0.0,
    verbose = TRUE,
    loss_options = list(),
    optim_options = list(),
    path = "Null")

{
  verbose <- ifelse(verbose,"true","false")
  JuliaCall::julia_assign("loss_options",loss_options)
  JuliaCall::julia_assign("optim_options",optim_options)

  print(julia_eval("import.Pkg;Pkg.status(\"UniversalDiffEq\")"))
  JuliaCall::julia_eval(paste0("cv_data=UniversalDiffEq.leave_future_out(",model,", training!, ", k,")"),
             need_return = "R")
  print("Done! :)")
  return(cv_data)
}

