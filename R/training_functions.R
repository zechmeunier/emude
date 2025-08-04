#' Train the UDE
#'
#' `train_UDE` trains the UDE model on the training data provided, with several
#'  options for the loss function and optimization algorithm (see `loss_function`
#'  and `optimizer` below). These methods trade off between accuracy, stability,
#'  and computing time. Their performance may also be related to the
#'  characteristics of the training data. For additional details see the
#'  [Julia package documentation](https://jack-h-buckner.github.io/UniversalDiffEq.jl/dev/TrainingRoutines/)
#'
#' @param model description
#' @param loss_function One of five possible loss functions:
#' - `conditional likelihood` for a state-space training process with joint
#' likelihoods (it needs to be renamed in UniversalDiffEq.jl).
#' - `marginal likelihood` for a state-space training process with marginal
#' likelihoods.
#' - `derivative matching` for a fast, two-step training process. First, a
#' smoothing function is fit to the data using a spline regression. Then, the UDE
#' model is trained by comparing the derivatives of the smoothing functions to
#' the derivatives predicted by the right-hand side of the UDE. It is the default
#' because it is the fastest method, but it is **not** the most accurate.
#' - `shooting`
#' - `multiple shooting`
#'
#' | Loss Function          | Discrete Model | Continuous Model | Speed    |
#' |------------------------|----------------|------------------|----------|
#' | Conditional likelihood | Yes            | Yes              | Moderate |
#' | Marginal likelihood    | Yes            | Yes              | Slow     |
#' | Derivative matching    | No             | Yes              | Fast     |
#' | Shooting               | No             | Yes              | Moderate |
#' | Multiple shooting      | No             | Yes              | Moderate |
#' @param optimizer description
#' @param regularization description
#' @param verbose description
#' @param loss_options description
#' @param optim_options description
#' @return description
#' @export
train_UDE <- function(
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
