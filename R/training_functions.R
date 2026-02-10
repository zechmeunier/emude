#' Train the UDE
#'
#' `train_UDE()` trains the UDE model on the training data provided, with several
#'  options for the loss function and optimization algorithm (see `loss_function`
#'  and `optimizer`). These methods trade off between accuracy, stability,
#'  and computing time. Their performance may also be related to the
#'  characteristics of the training data and hyperparameters (see `loss_options`
#'  and `optim_options`). For additional details see the
#'  [Julia package documentation](https://jack-h-buckner.github.io/UniversalDiffEq.jl/dev/TrainingRoutines/).
#'
#' @param model A UDE model created with one of the model constructor functions:
#' `NODE()`, `multi_NODE()`, `custom_derivatives()`, or `multi_custom_derivatives()`.
#'
#' @param loss_function One of five possible loss functions:
#' - `joint likelihood` for a state-space training process calculating the joint likelihood
#' of the data given the state estimates and point estimates of the parameters.
#' User can supply the values of observation error and process error in the
#' `loss_options` argument.
#' - `marginal likelihood` for a state-space training process calculating the
#' marginal likelihood of the observed data given point estimates of the parameters.
#' This method accounts for uncertainty over the estimated states, but is much slower.
#' User can supply the values of observation error and process error in the
#' `loss_options` argument.
#' - `derivative matching` for a non-state-space training process with two steps. First, a
#' smoothing function is fit to the data using a spline regression. Then, the UDE
#' model is trained by comparing the derivatives of the smoothing functions to
#' the derivatives predicted by the right-hand side of the UDE. It is the default
#' because it is the fastest method, but it is **not** always the most accurate.
#' - `shooting` for a non-state-space training process that simulates a solution
#' to the ODE model over the full length of the training set with smooth changes
#' over time. It has difficulty escaping local minimum solutions of datasets
#' with oscillations. This method is not suitable for chaotic dynamics because of
#' the sensitivity of the simulation trajectories to the initial conditions and model parameters.
#' - `multiple shooting` for a non-state-space training process that simulates solutions
#' over smaller sections of the dataset and avoids the local minimum solutions
#' that hinder the performance of the shooting loss function.
#'
#' | **Loss Function**   |**Discrete Model**|**Continuous Model**|**Speed**|
#' |---------------------|------------------|--------------------|---------|
#' | Joint likelihood    | Yes              | Yes                | Moderate|
#' | Marginal likelihood | Yes              | Yes                | Slow    |
#' | Derivative matching | No               | Yes                | Fast    |
#' | Shooting            | No               | Yes                | Moderate|
#' | Multiple shooting   | No               | Yes                | Moderate|
#'
#' @param optimizer One of two possible optimization algorithms:
#' - `Adam` optimization is a stochastic gradient descent method that is based on
#' adaptive estimation of first moment (mean) and second moment (variance). It
#' works well with large datasets and complex models because it uses memory
#' efficiently and adapts the learning rate for each parameter automatically.
#' - `BFGS` optimization uses a second-order information approximation to minimize
#' the loss function. This method can result in overfitting if the neural network
#' is not sufficiently regularized.
#' @param regularization_weight Weight given to the L2 regularization penalty term,
#' which reduces overfitting of the model to training data. The default is 0, and
#' suggested weights range from \eqn{10^{-4}} to \eqn{10^4} in order of magnitude increments.
#' @param loss_options Optional settings for loss function tuning, with specific
#' hyperparameters for each loss function:
#' - `joint likelihood` and `marginal likelihood` have hyperparameters `process_error`
#' and `observation_error`. The user can supply a decimal, a vector of length \eqn{n},
#' or a positive definite \eqn{n \times n} matrix. If a decimal is provided, the error matrix
#' will use that value along the diagonal and set all covariance terms to zero.
#' If a vector is provided, it will be used as the diagonal of the matrix with all
#' other terms equal to zero, and if a matrix is provided, it will be used as the
#' full error covariance matrix. Note that if only one hyperparameter is provided,
#' it needs to be followed or preceded by a comma (e.g., `loss_options = list(process_error = 0.025,)`).
#' - `derivative matching` has hyperparameters `d` and `remove_ends`. The
#' hyperparameter `d` sets the number of degrees of freedom used by
#' the curve-fitting model. The `remove_ends` hyperparameter allows data
#' points from the beginning and end of the dataset to be excluded from the loss
#' function. The default value is zero (no observations are excluded), but the
#' smoothing curves might fit poorly near the beginning and end of some datasets.
#' Note that if only one hyperparameter is provided, it needs to be followed or preceded
#' by a comma (e.g., `loss_options = list(d = 12,)`).
#' - `shooting` has no optional hyperparameters.
#' - `multiple shooting` has hyperparameter `pred_length`. This hyperparameter is the number
#' of data points spanned by each prediction interval. The default value is 5.
#' @param optim_options Optional settings for optimization algorithm tuning, with specific
#' hyperparameters for each optimization algorithm:
#' - `Adam` has hyperparameters `step_size` and `maxiter`. The defaults for these
#' hyperparameters are set to values that perform well with each loss function.
#' Increasing the maximum number of iterations `maxiter` often improves model fits.
#' The beginning learning rate `step_size` controls how much model weights are
#' adjusted with each iteration of the algorithm, but Adam adjusts this automatically.
#' - `BFGS` has hyperparameter `initial_step_norm` to approximate the initial inverse Hessian.
#' @return A trained UDE model.
#' @export
train_UDE <- function(
    model,
    loss_function = "derivative matching",
    optimizer = "Adam",
    regularization_weight = 0.0,
    loss_options = list(),
    optim_options = list())

{
  if(loss_function == "joint likelihood"){
    loss_function <- "conditional likelihood"
  }
  if(loss_function == "marginal likelihood" &&
     (is.null(loss_options) || !is.numeric(loss_options$observation_error))){
    warning("No value of observation error supplied for training.",
            "Using the default value of 0.1 for observation error, which may be inappropriate for your data.\n")
  }
  if(optimizer == "Adam"){
    optimizer <- "ADAM"
  }

  verbose <- ifelse(verbose,"true","false")
  JuliaCall::julia_assign("loss_options",loss_options)
  JuliaCall::julia_assign("optim_options",optim_options)
  JuliaCall::julia_eval(paste0("train!(",model,
                               ",loss_function=","\"", loss_function, "\"",
                               ",optimizer=","\"",optimizer,"\"",
                               ",regularization_weight=",regularization_weight,
                               ",verbose=",verbose,
                               ",loss_options=NamedTuple(loss_options)",
                               ",optim_options=NamedTuple(optim_options))"))
  print("Done! :)")
}
