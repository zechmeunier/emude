#' Run Flexible Lotka-Volterra Simulation
#'
#' @param initial_state Numeric vector: c(prey_val, pred_val)
#' @param times Numeric vector: sequence of time points
#' @param param List: contains growth rates, mortality, and model switches
#' @param nn Optional: Neural network object or pre-calculated consumption vector
#' @return A data frame of the simulation results
run_lv_model <- function(initial_state, times, param, nn = NULL) {

  # 1. Run validation once before starting the solver
  validate_params(param)

  # 2. Call the ODE solver
  # We pass 'nn' as an additional argument to the solver
  out <- deSolve::ode(
    y = initial_state,
    times = times,
    func = lv_derivs,
    parms = param,
    nn = nn
  )

  return(as.data.frame(out))
}

#' Internal ODE Function
lv_derivs <- function(t, state, param, nn) {
  N <- state[1]
  P <- state[2]

  # --- 1. Functional Response (Consumption) ---
  # If the user provides a neural network (nn), it overrides the analytic types.
  consumption <- if (!is.null(nn)) {
    # If nn is a function (like a trained model), we call it: nn(state, param)
    # If it's just a value/vector, we use it directly.
    if (is.function(nn)) nn(state, param) else nn[1]
  } else {
    switch(param$fun_resp,
           "type1" = param$a * N * P,
           "type2" = (param$a * N / (1 + param$a * param$h * N)) * P,
           "type3" = (param$a * N^2 / (1 + param$a * param$h * N^2)) * P
    )
  }

  # --- 2. Prey Growth ---
  dN_growth <- if (param$prey_growth == "logistic") {
    param$r_n * N * (1 - N / param$K_n)
  } else {
    param$r_n * N
  }

  # --- 3. Predator Growth ---
  dP_growth <- if (param$pred_growth == "logistic") {
    param$r_p * P * (1 - P / param$K_p)
  } else if (param$pred_growth == "exponential") {
    param$r_p * P
  } else {
    0
  }

  # --- 4. ODE Equations ---
  du1 <- dN_growth - consumption
  du2 <- (param$delta * consumption) + dP_growth - (param$m * P)

  list(c(du1, du2))
}
