#' Perform leave-future-out cross-validation
#'
#' `cross_validation()` runs leave-future-out cross-validation on the UDE model
#' using a training routine with *k* folds.
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
#' @param path File path for saving a .csv file containing the raw testing data
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
#' df <- data.frame("time" = seq(1,40),
#'                  "speciesA" = rpois(40,20),
#'                  "speciesB" = rpois(40,40))
#' NODE_model <- NODE(data = df, time_column_name = "time")
#' cv_results <- cross_validation(model = NODE_model, k = 5)
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
    path = NULL)

{
  verbose <- ifelse(verbose,"true","false")
  JuliaCall::julia_assign("loss_options",loss_options)
  JuliaCall::julia_assign("optim_options",optim_options)
  JuliaCall::julia_eval(paste0("function training!(model) ",
                               "train!(", model,
                               ",loss_function=","\"", loss_function, "\"",
                               ",optimizer=","\"", optimizer, "\"",
                               ",regularization_weight=", regularization_weight,
                               ",verbose=", verbose,
                               ",loss_options=NamedTuple(loss_options)",
                               ",optim_options=NamedTuple(optim_options))",
                               "end"))

  print(julia_eval("import.Pkg;Pkg.status(\"UniversalDiffEq\")"))
  cv_results <- list()

  if(!is.null(path)) {
    cv_results <-
      JuliaCall::julia_eval(paste0("leave_future_out(",
                                   model,
                                   ",training!",
                                   ",", k,
                                   ",path=", "\"", path, "\")"),
                            need_return = "R")
  }
  else {
    cv_results <-
      JuliaCall::julia_eval(paste0("leave_future_out(",
                                   model,
                                   ",training!",
                                   ",", k),
                            need_return = "R")
  }
  print("Done! :)")
  return(cv_results)
}


#' Get the right-hand side of the UDE model
#'
#' `get_right_hand_side()` creates an R function that returns the right-hand side
#' of the UDE model given the time `t`, state `u`, and covariates `X` if included
#' in the model.
#'
#' @param model The UDE model to evaluate.
#'
#' @return A function that returns the value of the ODE model. The function will
#' require different arguments depending on the UDE model. If the UDE does not have
#' covariates, then the right-hand side function will take two arguments `(t,u)`, where
#' `u` is a vector with the value of the state variables and `t` is time. If covariates
#' are used, then the arguments are `(t,u,X)`, where `X` is a vector of the covariates.
#' @export
#' @import deSolve
#'
#' @examples
#' df <- data.frame("time" = seq(1,40),
#'                 "speciesA" = rpois(40,20),
#'                 "speciesB" = rpois(40,40))
#' NODE_model <- NODE(data = df, time_column_name = "time")
#' train_UDE(NODE_model)
#' rhs <- get_right_hand_side(NODE_model)
#' u <- c(1.1,2)
#' t <- 0.0
#' rhs(u,t) # evaluates
#' # Simulate solutions of UDE model with deSolve
#' library(deSolve)
#' ode(y=as.u, times = seq(0,10.0,0.2), parms = c(),
#'    func = function(t,u,pars){list(rhs(t,u))}, method = "lsoda")
get_right_hand_side <- function(model){

  JuliaCall::julia_eval(
    paste0("rhs_",model,"=get_right_hand_side(",model,")")
  )

  covars = JuliaCall::julia_eval(paste0("typeof(",model, ".X", ")"), need_return = "R")

  if(as.character(covars) != "DataFrames.DataFrame"){
    print("Returning ODE of form: function(t,u)")
    rhs <- function(t,u){
      JuliaCall::julia_assign("u",u)
      JuliaCall::julia_assign("t",t)
      JuliaCall::julia_eval(
        paste0("rhs_",model, "(u,t)"),
        need_return = "R")
    }
  }else{
    print("Returning ODE of form: function(t,u,x)")
    rhs <- function(t,u,x){
      JuliaCall::julia_assign("u",u)
      JuliaCall::julia_assign("x",x)
      JuliaCall::julia_assign("t",t)
      JuliaCall::julia_eval(
        paste0("rhs_",model, "(u,x,t)"),
        need_return = "R")
    }
  }
  return(rhs)
}
