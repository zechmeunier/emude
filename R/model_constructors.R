#' Define a NODE model with one time series
#'
#' `NODE()` constructs a neural ordinary differential equation (NODE) model. NODEs
#' use neural networks to learn unknown nonlinear relationships from time series
#' data. `NODE()` builds a continuous-time UDE for state variables \eqn{u_t} and
#' covariates \eqn{x_t} using a neural network, with weights \eqn{w} and
#' biases \eqn{b}, to represent the right-hand side of the differential equation
#' \deqn{\frac{du}{dt} = NN(u_t,x_t;w,b)}
#'
#' @param data A data frame of observed state variables over time.
#' @param covariates A data frame of observed covariates (e.g., environmental
#' conditions) over time. This data frame must have the same column
#' name for time as the primary dataset, but the time points do not need to
#' match because the values of the covariates between time points included in
#' the data frame `covariates` are interpolated using a linear spline. Optional.
#' @param time_column_name The column in `data` and `covariates` that contains
#' the time data, indicating when the observations were made.
#' @param hidden_units Number of neurons in the single hidden layer.
#' @param seed Fixed random seed for repeatable results.
#' @param proc_weight Weight of the process error term \eqn{\nu_t} in the loss
#' function. The process weight controls how closely the model predictions
#' match the state estimates \eqn{\hat{u}_t}.
#' @param obs_weight Weight of the observation error term \eqn{\epsilon_t} in the loss
#' function. The observation weight controls how closely the state estimates
#' \eqn{\hat{u}_t} match the observations \eqn{y_t}. Smaller values of the observation weight
#' correspond to datasets with larger amounts of observation error and vice versa.
#' @param reg_weight Weight \eqn{\lambda} of the regularization penalty term in the loss
#' function.
#' @param reg_type Type of regularization used to mitigate overfitting.
#' Options are either "L1" (LASSO) or "L2" (ridge regression). The penalty term
#' added to the loss function is either the absolute value of the sum of
#' coefficients (L1) or the squared sum of coefficients (L2). Generally, the
#' default of "L2" should be used.
#' @param l Extrapolation length scale parameter for forecasting. `l` controls
#' how quickly correlations decay with distance between points (i.e., how wiggly the function is).
#' Small values lead to fast decay and the extrapolation reverts to the prior mean
#' quickly beyond the observed data. Large values lead to slow decay and the extrapolation
#' stays similar to the last trend for a longer period.
#' @param extrap_rho Extrapolation marginal SD parameter for forecasting.
#' `extrap_rho` controls the magnitude of the extrapolation. Small values lead to
#' narrow confidence intervals, large values lead to wide confidence intervals.
#' @param bayesian Logical (`TRUE` or `FALSE`) for whether or not the UDE is a
#' Bayesian UDE.
#' @param uid A string that serves as a unique identifier to save the
#' model into Julia. It is not recommended to modify this parameter.
#'
#' @return An untrained NODE model containing all the defined parameters.
#'
#' @export
#'
#' @examples
#' print("test")
NODE <- function(
    data,
    covariates = NULL,
    time_column_name = "time",
    hidden_units = 10,
    seed = 1,
    proc_weight = 1.0,
    obs_weight = 1.0,
    reg_weight = 10^-6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.1,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){
  if (sd(as.matrix(data[, setdiff(names(data), time_column_name)]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
  model_type <- ifelse(bayesian,"BayesianNODE","NODE")
  JuliaCall::julia_assign("data_julia",convert_column_types(data))
  if(is.null(covariates)){
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=", model_type,
                                 "(data_julia,time_column_name=\"",time_column_name,"\"",
                                 ",hidden_units=",hidden_units,
                                 ",seed=",seed,
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",reg_weight=",reg_weight,
                                 ",reg_type=\"",reg_type,"\"",
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }else{
    JuliaCall::julia_assign("covariates_julia",covariates)
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=",model_type,
                                 "(data_julia,covariates_julia,time_column_name=\"",time_column_name,"\"",
                                 ",hidden_units=",hidden_units,
                                 ",seed=",seed,
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",reg_weight=",reg_weight,
                                 ",reg_type=\"",reg_type,"\"",
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }
  return(paste0("julia_model_",uid))
}


#' Define a NODE model with multiple time series
#'
#' `multi_NODE()` constructs a neural ordinary differential equation (NODE) model
#' for multiple time series. NODEs use neural networks to learn unknown
#' nonlinear relationships from time series data. `multi_NODE()` builds
#' a continuous-time UDE for state variables \eqn{u_t} and covariates \eqn{x_t}
#' in series \eqn{i} using a neural network, with weights \eqn{w} and
#' biases \eqn{b}, to represent the right-hand side of the differential equation
#' \deqn{\frac{du}{dt} = NN(u_{i,t},x_{i,t};w,b)}
#'
#' @param data A data frame of observed state variables over time.
#' @param covariates A data frame of observed covariates (e.g., environmental
#' conditions) over time. This data frame must have the same column
#' name for time as the primary dataset, but the time points do not need to
#' match because the values of the covariates between time points included in
#' the data frame `covariates` are interpolated using a linear spline. Optional.
#' @param time_column_name The column in `data` and `covariates` that contains
#' the time data, indicating when the observations were made.
#' @param series_column_name The column in `data` and `covariates` that contains
#' the series data, indicating the identifying information for the observations.
#' @param hidden_units Number of neurons in the single hidden layer.
#' @param seed Fixed random seed for repeatable results.
#' @param proc_weight Weight of the process error term \eqn{\nu_t} in the loss
#' function. The process weight controls how closely the model predictions
#' match the state estimates \eqn{\hat{u}_t}.
#' @param obs_weight Weight of the observation error term \eqn{\epsilon_t} in the loss
#' function. The observation weight controls how closely the state estimates
#' \eqn{\hat{u}_t} match the observations \eqn{y_t}. Smaller values of the observation weight
#' correspond to datasets with larger amounts of observation error and vice versa.
#' @param reg_weight Weight \eqn{\lambda} of the regularization penalty term in the loss
#' function.
#' @param reg_type Type of regularization used to mitigate overfitting.
#' Options are either "L1" (LASSO) or "L2" (ridge regression). The penalty term
#' added to the loss function is either the absolute value of the sum of
#' coefficients (L1) or the squared sum of coefficients (L2). Generally, the
#' default of "L2" should be used.
#' @param l Extrapolation length scale parameter for forecasting. `l` controls
#' how quickly correlations decay with distance between points (i.e., how wiggly the function is).
#' Small values lead to fast decay and the extrapolation reverts to the prior mean
#' quickly beyond the observed data. Large values lead to slow decay and the extrapolation
#' stays similar to the last trend for a longer period.
#' @param extrap_rho Extrapolation marginal SD parameter for forecasting.
#' `extrap_rho` controls the magnitude of the extrapolation. Small values lead to
#' narrow confidence intervals, large values lead to wide confidence intervals.
#' @param bayesian Logical (`TRUE` or `FALSE`) for whether or not the UDE is a
#' Bayesian UDE.
#' @param uid A string that serves as a unique identifier to save the
#' model into Julia. It is not recommended to modify this parameter.
#'
#' @return An untrained NODE model containing all the defined parameters.
#' @export
#'
#' @examples
#' print("test")
multi_NODE <- function(
    data,
    covariates = NULL,
    time_column_name = "time",
    series_column_name = "series",
    hidden_units = 10,
    seed = 1,
    proc_weight = 1.0,
    obs_weight = 1.0,
    reg_weight = 10^-6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.1,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){

  if (sd(as.matrix(data[, setdiff(names(data), c(time_column_name, series_column_name))]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
  model_type <- ifelse(bayesian,"BayesianNODE","MultiNODE")
  JuliaCall::julia_assign("data_julia",convert_column_types(data))
  if(is.null(covariates)){
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=",model_type,
                                 "(data_julia,time_column_name=\"",time_column_name,"\"",
                                 ",series_column_name=\"",series_column_name,"\"",
                                 ",hidden_units=",hidden_units,
                                 ",seed=",seed,
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",reg_weight=",reg_weight,
                                 ",reg_type=\"",reg_type,"\"",
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }else{
    JuliaCall::julia_assign("covariates_julia",covariates)
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=",model_type,
                                 "(data_julia,covariates_julia,time_column_name=\"",time_column_name,"\"",
                                 ",series_column_name=\"",series_column_name,"\"",
                                 ",hidden_units=",hidden_units,
                                 ",seed=",seed,
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",reg_weight=",reg_weight,
                                 ",reg_type=\"",reg_type,"\"",
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }
  return(paste0("julia_model_",uid))
}


#' Define a custom derivatives UDE with one time series
#'
#' `custom_derivatives()` constructs a universal differential equation (UDE) model
#' based on known functional forms in a user-defined derivatives function `derivs`.
#' These models embed neural networks in the right-hand side of a system of differential equations
#' \deqn{\frac{du}{dt}=f(u_t,x_t,t,NN(u_t,x_t);\theta)},
#' where \eqn{u_t} is a vector of state variables, \eqn{x_t} is a vector of covariates,
#' \eqn{t} is time, \eqn{NN} is the output layer of a neural network, and
#' \eqn{\theta} is a set of parameters including the weights and biases of the neural network.
#'
#' @param data A data frame of observed state variables over time.
#' @param derivs A user-defined function of the form `derivs(u,nn,p,t)` where
#' `u` stores the value of the state variables, `nn` stores the neural network
#' outputs, `p` stores the model parameters, and `t` is time. The function should
#' save each ODE to `du[i]`, where `i` is an index for each time derivative.
#' @param initial_parameters A named list containing the model parameters stored
#' in `p`.
#' @param covariates A data frame of observed covariates (e.g., environmental
#' conditions) over time. This data frame must have the same column
#' name for time as the primary dataset, but the time points do not need to
#' match because the values of the covariates between time points included in
#' the data frame `covariates` are interpolated using a linear spline. Optional.
#' @param neural_network_inputs The number of input nodes of the neural network.
#' @param neural_network_outputs The number of output nodes of the neural network.
#' @param hidden_units Number of neurons in the single hidden layer.
#' @param time_column_name The column in `data` and `covariates` that contains
#' the time data, indicating when the observations were made.
#' @param proc_weight Weight of the process error term \eqn{\nu_t} in the loss
#' function. The process weight controls how closely the model predictions
#' match the state estimates \eqn{\hat{u}_t}.
#' @param obs_weight Weight of the observation error term \eqn{\epsilon_t} in the loss
#' function. The observation weight controls how closely the state estimates
#' \eqn{\hat{u}_t} match the observations \eqn{y_t}. Smaller values of the observation weight
#' correspond to datasets with larger amounts of observation error and vice versa.
#' @param reg_weight Weight \eqn{\lambda} of the regularization penalty term in the loss
#' function.
#' @param reg_type Type of regularization used to mitigate overfitting.
#' Options are either "L1" (LASSO) or "L2" (ridge regression). The penalty term
#' added to the loss function is either the absolute value of the sum of
#' coefficients (L1) or the squared sum of coefficients (L2). Generally, the
#' default of "L2" should be used.
#' @param l Extrapolation length scale parameter for forecasting. `l` controls
#' how quickly correlations decay with distance between points (i.e., how wiggly the function is).
#' Small values lead to fast decay and the extrapolation reverts to the prior mean
#' quickly beyond the observed data. Large values lead to slow decay and the extrapolation
#' stays similar to the last trend for a longer period.
#' @param extrap_rho Extrapolation marginal SD parameter for forecasting.
#' `extrap_rho` controls the magnitude of the extrapolation. Small values lead to
#' narrow confidence intervals, large values lead to wide confidence intervals.
#' @param bayesian Logical (`TRUE` or `FALSE`) for whether or not the UDE is a
#' Bayesian UDE.
#' @param uid A string that serves as a unique identifier to save the
#' model into Julia. It is not recommended to modify this parameter.
#'
#' @return An untrained custom derivatives UDE model containing all the defined parameters.
#' @export
#'
#' @examples
#' print("test")
custom_derivatives <- function(
    data,
    derivs,
    initial_parameters,
    covariates = NULL,
    neural_network_inputs = c(1),
    neural_network_outputs = 1,
    hidden_units = 10,
    time_column_name = "time",
    proc_weight = 1.0,
    obs_weight = 1.0,
    reg_weight = 10^-6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.1,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){

  if (sd(as.matrix(data[, setdiff(names(data), time_column_name)]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
  model_type <- ifelse(bayesian,"BayesianUDE","CustomDerivatives")

  if(is.character(derivs)) {
    JuliaCall::julia_eval(paste0('include("', derivs, '")'))
    JuliaCall::julia_eval("f_julia = derivs")
  }
  else{
    translated_function <- R_to_Julia(derivs)
    JuliaCall::julia_eval(paste("f_julia = ", translated_function))
  }


  JuliaCall::julia_assign("p_julia",initial_parameters)
  JuliaCall::julia_eval("p_julia = NamedTuple(p_julia)", need_return = "Julia")

  JuliaCall::julia_assign("data_julia",convert_column_types(data))
  JuliaCall::julia_assign("inputs",neural_network_inputs)
  JuliaCall::julia_assign("outputs",neural_network_outputs)
  JuliaCall::julia_assign("hidden_units",hidden_units)

  JuliaCall::julia_eval("deriv, parameters = build_custom_derivs_function_R(f_julia,p_julia,inputs,hidden_units,outputs)")

  if(is.null(covariates)){
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=",model_type,
                                 "(data_julia,deriv,parameters,time_column_name=\"",time_column_name,"\"",
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",reg_weight=",reg_weight,
                                 ",reg_type=\"",reg_type,"\"",
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }else{
    JuliaCall::julia_assign("covariates_julia",covariates)
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=",model_type,
                                 "(data_julia,covariates_julia,deriv,parameters,time_column_name=\"",time_column_name,"\"",
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",reg_weight=",reg_weight,
                                 ",reg_type=\"",reg_type,"\"",
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }
  return(paste0("julia_model_",uid))
}


#' Define a custom derivatives UDE with multiple time series
#'
#' `multi_custom_derivatives()` constructs a universal differential equation (UDE) model
#' for multiple time series
#' based on known functional forms in a user-defined derivatives function `derivs`.
#' These models embed neural networks in the right-hand side of a system of differential equations
#' \deqn{\frac{du}{dt}=f(u_{i,t},x_{i,t},i,t,NN(u_{i,t},x_{i,t});\theta)},
#' where \eqn{u_t} is a vector of state variables, \eqn{x_t} is a vector of covariates,
#' \eqn{t} is time, \eqn{i} is series, \eqn{NN} is the output layer of a neural network, and
#' \eqn{\theta} is a set of parameters including the weights and biases of the neural network.
#'
#' @param data A data frame of observed state variables over time.
#' @param derivs A user-defined function of the form `derivs(u,nn,p,t)` where
#' `u` stores the value of the state variables, `nn` stores the neural network
#' outputs, `p` stores the model parameters, and `t` is time. The function should
#' save each ODE to `du[i]`, where `i` is an index for each time derivative.
#' @param initial_parameters A named list containing the model parameters stored
#' in `p`.
#' @param covariates A data frame of observed covariates (e.g., environmental
#' conditions) over time. This data frame must have the same column
#' name for time as the primary dataset, but the time points do not need to
#' match because the values of the covariates between time points included in
#' the data frame `covariates` are interpolated using a linear spline. Optional.
#' @param neural_network_inputs The number of input nodes of the neural network.
#' @param neural_network_outputs The number of output nodes of the neural network.
#' @param hidden_units Number of neurons in the single hidden layer.
#' @param time_column_name The column in `data` and `covariates` that contains
#' the time data, indicating when the observations were made.
#' @param series_column_name The column in `data` and `covariates` that contains
#' the series data, indicating the identifying information for the observations.
#' @param proc_weight Weight of the process error term \eqn{\nu_t} in the loss
#' function. The process weight controls how closely the model predictions
#' match the state estimates \eqn{\hat{u}_t}.
#' @param obs_weight Weight of the observation error term \eqn{\epsilon_t} in the loss
#' function. The observation weight controls how closely the state estimates
#' \eqn{\hat{u}_t} match the observations \eqn{y_t}. Smaller values of the observation weight
#' correspond to datasets with larger amounts of observation error and vice versa.
#' @param reg_weight Weight \eqn{\lambda} of the regularization penalty term in the loss
#' function.
#' @param reg_type Type of regularization used to mitigate overfitting.
#' Options are either "L1" (LASSO) or "L2" (ridge regression). The penalty term
#' added to the loss function is either the absolute value of the sum of
#' coefficients (L1) or the squared sum of coefficients (L2). Generally, the
#' default of "L2" should be used.
#' @param l Extrapolation length scale parameter for forecasting. `l` controls
#' how quickly correlations decay with distance between points (i.e., how wiggly the function is).
#' Small values lead to fast decay and the extrapolation reverts to the prior mean
#' quickly beyond the observed data. Large values lead to slow decay and the extrapolation
#' stays similar to the last trend for a longer period.
#' @param extrap_rho Extrapolation marginal SD parameter for forecasting.
#' `extrap_rho` controls the magnitude of the extrapolation. Small values lead to
#' narrow confidence intervals, large values lead to wide confidence intervals.
#' @param bayesian Logical (`TRUE` or `FALSE`) for whether or not the UDE is a
#' Bayesian UDE.
#' @param uid A string that serves as a unique identifier to save the
#' model into Julia. It is not recommended to modify this parameter.
#'
#' @return An untrained custom derivatives UDE model containing all the defined parameters.
#' @export
#'
#' @examples
#' print("test")
multi_custom_derivatives <- function(
    data,
    derivs,
    initial_parameters,
    covariates = NULL,
    neural_network_inputs = c(1),
    neural_network_outputs = 1,
    hidden_units = 10,
    time_column_name = "time",
    series_column_name = "series",
    proc_weight = 1.0,
    obs_weight = 1.0,
    reg_weight = 10^-6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.1,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){

  if (sd(as.matrix(data[, setdiff(names(data), c(time_column_name, series_column_name))]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
  model_type <- ifelse(bayesian,"BayesianUDE","MultiCustomDerivatives")

  if(is.character(derivs)) {
    JuliaCall::julia_eval(paste0('include("', derivs, '")'))
    JuliaCall::julia_eval("f_julia = derivs")
  }
  else{
    translated_function <- R_to_Julia(derivs)
    JuliaCall::julia_eval(paste("f_julia = ", translated_function))
  }


  JuliaCall::julia_assign("p_julia",initial_parameters)
  JuliaCall::julia_eval("p_julia = NamedTuple(p_julia)", need_return = "Julia")

  JuliaCall::julia_assign("data_julia",convert_column_types(data))
  JuliaCall::julia_assign("inputs",neural_network_inputs)
  JuliaCall::julia_assign("outputs",neural_network_outputs)
  JuliaCall::julia_assign("hidden_units",hidden_units)

  JuliaCall::julia_eval("deriv, parameters = build_multi_custom_derivs_function_R(f_julia,p_julia,inputs,hidden_units,outputs)")

  if(is.null(covariates)){
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=",model_type,
                                 "(data_julia,deriv,parameters,time_column_name=\"",time_column_name,"\"",
                                 ",series_column_name=\"",series_column_name,"\"",
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",reg_weight=",reg_weight,
                                 ",reg_type=\"",reg_type,"\"",
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }else{
    JuliaCall::julia_assign("covariates_julia",covariates)
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=",model_type,
                                 "(data_julia,covariates_julia,deriv,parameters,time_column_name=\"",time_column_name,"\"",
                                 ",series_column_name=\"",series_column_name,"\"",
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",reg_weight=",reg_weight,
                                 ",reg_type=\"",reg_type,"\"",
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }
  return(paste0("julia_model_",uid))
}


#' Define a custom derivatives model with no neural network
#'
#' `ode_model()` constructs an ordinary differential equation (ODE) model. It
#' does not train a neural network and can be used as a null model.
#'
#' @param data A data frame of observed state variables over time.
#' @param derivs A user-defined function of the form `derivs(u,nn,p,t)` where
#' `u` stores the value of the state variables, `nn` stores the neural network
#' outputs, `p` stores the model parameters, and `t` is time. The function should
#' save each ODE to `du[i]`, where `i` is an index for each time derivative.
#' @param initial_parameters A named list containing the model parameters stored
#' in `p`.
#' @param covariates A data frame of observed covariates (e.g., environmental
#' conditions) over time. This data frame must have the same column
#' name for time as the primary dataset, but the time points do not need to
#' match because the values of the covariates between time points included in
#' the data frame `covariates` are interpolated using a linear spline. Optional.
#' @param time_column_name The column in `data` and `covariates` that contains
#' the time data, indicating when the observations were made.
#' @param proc_weight Weight of the process error term \eqn{\nu_t} in the loss
#' function. The process weight controls how closely the model predictions
#' match the state estimates \eqn{\hat{u}_t}.
#' @param obs_weight Weight of the observation error term \eqn{\epsilon_t} in the loss
#' function. The observation weight controls how closely the state estimates
#' \eqn{\hat{u}_t} match the observations \eqn{y_t}. Smaller values of the observation weight
#' correspond to datasets with larger amounts of observation error and vice versa.
#' @param l Extrapolation length scale parameter for forecasting. `l` controls
#' how quickly correlations decay with distance between points (i.e., how wiggly the function is).
#' Small values lead to fast decay and the extrapolation reverts to the prior mean
#' quickly beyond the observed data. Large values lead to slow decay and the extrapolation
#' stays similar to the last trend for a longer period.
#' @param extrap_rho Extrapolation marginal SD parameter for forecasting.
#' `extrap_rho` controls the magnitude of the extrapolation. Small values lead to
#' narrow confidence intervals, large values lead to wide confidence intervals.
#' @param bayesian Logical (`TRUE` or `FALSE`) for whether or not the UDE is a
#' Bayesian UDE.
#' @param uid A string that serves as a unique identifier to save the
#' model into Julia. It is not recommended to modify this parameter.
#'
#' @return An untrained ODE model containing all the defined parameters.
#' @export
#'
#' @examples
#' print("test")
ode_model <- function(
    data,
    derivs,
    initial_parameters,
    covariates = NULL,
    time_column_name = "time",
    proc_weight = 1.0,
    obs_weight = 1.0,
    l = 10^3,
    extrap_rho = 0.1,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){

  if (sd(as.matrix(data[, setdiff(names(data), time_column_name)]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
  model_type <- ifelse(bayesian,"BayesianUDE","CustomDerivatives")

  if(is.character(derivs)) {
    JuliaCall::julia_eval(paste0('include("', derivs, '")'))
    JuliaCall::julia_eval("f_julia = derivs")
  }
  else{
    translated_function <- R_to_Julia(derivs)
    JuliaCall::julia_eval(paste("f_julia = ", translated_function))
  }

  JuliaCall::julia_assign("p_julia",initial_parameters)
  JuliaCall::julia_eval("p_julia = NamedTuple(p_julia)", need_return = "Julia")

  JuliaCall::julia_assign("data_julia",convert_column_types(data))

  julia_model <- julia_eval("deriv, parameters = build_custom_ode(f_julia,p_julia,inputs,outputs)")

  if(is.null(covariates)){
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=",model_type,
                                 "(data_julia,deriv,parameters,time_column_name=\"",time_column_name,"\"",
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }else{
    JuliaCall::julia_assign("covariates_julia",covariates)
    JuliaCall::julia_eval(paste0("julia_model_",uid,"=",model_type,
                                 "(data_julia,covariates_julia,deriv,parameters,time_column_name=\"",time_column_name,"\"",
                                 ",proc_weight=",proc_weight,
                                 ",obs_weight=",obs_weight,
                                 ",l=",l,
                                 ",extrap_rho=",extrap_rho,")"),
                          need_return = "Julia")
  }
  return(paste0("julia_model_",uid))
}
