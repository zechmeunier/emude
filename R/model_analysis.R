#' Leave-future-out cross-validation
#'
#' `cross_validation()` runs leave-future-out cross-validation on the UDE model
#' using a training routine with *k*-folds.
#'
#' @param model The UDE model to be tested with cross-validation (CV).
#' @param k The number of folds for CV going back *k* time steps. That is, a 5-fold
#' CV will sequentially validate the final 5 time steps. The default is 10 and
#' the recommended minimum number is 3. Fewer folds will perform faster but won't
#' validate as much of the time series.
#' @param loss_function The loss function for CV.
#' For more details, see \code{\link{train_UDE}}.
#' @param optimizer The optimization algorithm for CV.
#' For more details, see \code{\link{train_UDE}}.
#' @param regularization_weight The regularization weight for CV.
#' For more details, see \code{\link{train_UDE}}.
#' @param verbose Logical (`TRUE` or `FALSE`) for printing the training loss values
#' after each iteration. Currently not returning even when `TRUE`.
#' @param loss_options Loss function options for CV.
#' For more details, see \code{\link{train_UDE}}.
#' @param optim_options Optimizer options for CV.
#' For more details, see \code{\link{train_UDE}}.
#' @param path File path for saving .csv files containing the raw testing data
#' and forecasts for each fold.
#'
#' @return The function returns a list with two elements:
#' - The first contains an estimate of the mean absolute error (MAE) of the
#' forecasts and associated standard error as a function of the forecast horizon
#' (1 to *k* time steps into the future).
#' - The second contains two elements: `horizon_by_var` and `raw`. The data frame
#' `horizon_by_var` contains the forecasting errors separated by variable
#' and the data frame `raw` contains the raw testing and forecasting data. If
#' the model is trained on multiple time series, the second element will also include
#' a third data frame `horizon_by_var_by_series`.
#' @export
#'
#' @examples
#' X <- data.frame("time" = seq(1,40),
#'                 "speciesA" = rpois(40,20),
#'                 "speciesB" = rpois(40,40))
#' X_model <- NODE(data = X, time_column_name = "time")
#' cv_results <- cross_validation(model = X_model, k = 5)
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
    path = "nothing")

{
  verbose <- ifelse(verbose,"true","false")
  JuliaCall::julia_assign("loss_options",loss_options)
  JuliaCall::julia_assign("optim_options",optim_options)
  JuliaCall::julia_eval(paste0("function training!(model) ",
                               "train!(", model,
                               ",loss_function=","\"", loss_function, "\"",
                               ",optimizer=","\"", optimizer,"\"",
                               ",regularization_weight=", regularization_weight,
                               ",verbose=", verbose,
                               ",loss_options=NamedTuple(loss_options)",
                               ",optim_options=NamedTuple(optim_options))",
                               "end"))

  print(julia_eval("import.Pkg;Pkg.status(\"UniversalDiffEq\")"))
  cv_results <- list()
  cv_results <-
    JuliaCall::julia_eval(paste0("leave_future_out(",
                                 model,
                                 ",training!",
                                 ",", k,
                                 ",path=", "\"", path,"\")"),
                          need_return = "R")
  print("Done! :)")
  return(cv_results)
}
