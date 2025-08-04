#' Leave-future-out cross-validation
#'
#' `cross_validation()` runs leave-future-out cross-validation on the UDE model
#' using a training routine with *k*-folds.
#'
#' The function returns three data frames:
#' - The first contains an estimate of the mean absolute error of the
#' forecasts and associated standard error as a function of the forecast horizon
#' (1 to *k* time steps into the future).
#' - The second and third are returned in a
#' named tuple with two elements `horizon_by_var` and `raw`. The data frame
#' `horizon_by_var` contains the forecasting errors separated by variable
#' and the data frame `raw` contains the raw testing and forecasting data. If
#' the model is trained on multiple time series, the named tuple will include a
#' third data frame `horizon_by_var_by_series`.
#'
#' @param model The UDE model to be tested with cross-validation.
#' @param k The number of folds for cross-validation. The default is 10 and the
#' recommended minimum number is 3.
#' @param loss_function Missing description in Julia
#' @param optimizer Missing description in Julia
#' @param regularization_weight Missing description in Julia
#' @param verbose Missing description in Julia
#' @param loss_options Missing description in Julia
#' @param optim_options Missing description in Julia
#' @param path File path for saving .csv files containing the raw testing data
#' and forecasts for each fold.
#'
#' @return Several data frames.
#' @export
#'
#' @examples
#' print(x)
#'
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
  JuliaCall::julia_eval(paste0("function training!(model) ",
                               "train!(",
                               "model=", model,
                               ",loss_function=","\"", loss_function, "\"",
                               ",optimizer=","\"", optimizer,"\"",
                               ",regularization_weight=", regularization_weight,
                               ",verbose=", verbose,
                               ",loss_options=NamedTuple(loss_options)",
                               ",optim_options=NamedTuple(optim_options))",
                               "end"))

  print(julia_eval("import.Pkg;Pkg.status(\"UniversalDiffEq\")"))
  JuliaCall::julia_eval(paste0("cv_data = UniversalDiffEq.leave_future_out(",
                               "model=", model,
                               ",training!",
                               ",k=", k,
                               ",path=", path,")"),
                        need_return = "R")
  print("Done! :)")
  return(cv_data)
}
