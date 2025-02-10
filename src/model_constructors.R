source("src/helpers.R")
julia_eval("include(\"src/helpers.jl\")")
NODE <- function(
  data,
  covariates = NULL,
  time_column_name = "time",
  hidden_units = 10,
  seed = 1,
  proc_weight = 1.0,
  obs_weight = 1.0,
  reg_weight = 10 ^ -6,
  reg_type = "L2",
  l = 0.25,
  extrap_rho = 0.0,
  bayesian = FALSE
){
  model_type <- ifelse(bayesian,"BayesianNODE","NODE")
  julia_assign("data_julia",data)
  if(is.null(covariates)){
    julia_model <- julia_eval(paste0(model_type,
                     "(data_julia,time_column_name=\"",time_column_name,
                     "\",hidden_units=",hidden_units,
                     ",seed=",seed,
                     ",proc_weight=",proc_weight,
                     ",obs_weight=",obs_weight,
                     ",reg_weight=",reg_weight,
                     ",reg_type=\"",reg_type,
                     "\",l=",l,
                     ",extrap_rho=",extrap_rho,")"),
                    need_return = "Julia")

    julia_assign("julia_model_jl", julia_model)

  }else{
    julia_assign("covariates_julia",covariates)
    julia_model <- julia_eval(paste(model_type,
                     "(data_julia,covariates_julia,time_column_name=\"",time_column_name,
                     "\",hidden_units=",hidden_units,
                     ",seed=",seed,
                     ",proc_weight=",proc_weight,
                     ",obs_weight=",obs_weight,
                     ",reg_weight=",reg_weight,
                     ",reg_type=\"",reg_type,
                     "\",l=",l,
                     ",extrap_rho=",extrap_rho,")"),
                    need_return = "Julia")
  }

  # # train the model
  # julia_eval("train!(julia_model) # have a return conditon based on convergence")

  # # function to convert julia_model to something R can return 
  # R_model <- julia_eval("return_for_R(julia_model)")

  return(julia_model)
}

custom_derivatives <- function(
    data,
    derivs,
    initial_parameters,
    covariates = NULL,
    neural_network_inputs = 1,
    neural_network_outputs = 1,
    time_column_name = "time",
    hidden_units = 10,
    seed = 1,
    proc_weight = 1.0,
    obs_weight = 1.0,
    reg_weight = 10 ^ -6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.0,
    bayesian = FALSE
){
  model_type <- ifelse(bayesian,"BayesianUDE","CustomDerivatives")
  
  if(is.character(derivs)) {
    julia_eval(paste0('include("', derivs, '")'))
    julia_eval("f_julia = derivs")
  }
  else{
    translated_function <- R_to_Julia(derivs)
    julia_eval(paste("f_julia = ", translated_function))
  }

  
  julia_assign("p_julia",initial_parameters)
  julia_eval("p_julia = NamedTuple(p_julia)", need_return = "Julia")
  
  julia_assign("data_julia",data)
  julia_assign("inputs",neural_network_inputs)
  julia_assign("outputs",neural_network_outputs)
  
  julia_model <- julia_eval("deriv, parameters = build_custom_derivs_function_R(f_julia,p_julia,inputs,outputs)")
  
  if(is.null(covariates)){
    julia_model <- julia_eval(paste0(model_type,
                     "(data_julia,deriv,parameters,time_column_name=\"",time_column_name,
                     "\",hidden_units=",hidden_units,
                     ",seed=",seed,
                     ",proc_weight=",proc_weight,
                     ",obs_weight=",obs_weight,
                     ",reg_weight=",reg_weight,
                     ",reg_type=\"",reg_type,
                     "\",l=",l,
                     ",extrap_rho=",extrap_rho,")"),
                    need_return = "Julia")
  }else{
    julia_assign("covariates_julia",covariates)
    julia_model <- julia_eval(paste0(model_type,
                     "(data_julia,covariates_julia,deriv,parameters,time_column_name=\"",time_column_name,
                     "\",hidden_units=",hidden_units,
                     ",seed=",seed,
                     ",proc_weight=",proc_weight,
                     ",obs_weight=",obs_weight,
                     ",reg_weight=",reg_weight,
                     ",reg_type=\"",reg_type,
                     "\",l=",l,
                     ",extrap_rho=",extrap_rho,")"),
                    need_return = "Julia")
  }
  return(julia_model)
}

ode_model <- function(
    data,
    derivs,
    initial_parameters,
    covariates = NULL,
    time_column_name = "time",
    proc_weight = 1.0,
    obs_weight = 1.0,
    reg_weight = 10 ^ -6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.0,
    bayesian = FALSE
){
  model_type <- ifelse(bayesian,"BayesianUDE","CustomDerivatives")
  translated_function <- R_to_Julia(derivs)
  julia_eval(paste("f_julia = ", translated_function))
  
  julia_assign("p_julia",initial_parameters)
  julia_eval("p_julia = NamedTuple(p_julia)", need_return = "Julia")
  
  julia_assign("data_julia",data)
  julia_assign("inputs",neural_network_inputs)
  julia_assign("outputs",neural_network_outputs)
  
  julia_model <- julia_eval("deriv, parameters = build_custom_ode(f_julia,p_julia,inputs,outputs)")
  
  if(is.null(covariates)){
    julia_model <- julia_eval(paste0(model_type,
                     "(data_julia,deriv,parameters,time_column_name=\"",time_column_name,
                     "\",hidden_units=",hidden_units,
                     ",seed=",seed,
                     ",proc_weight=",proc_weight,
                     ",obs_weight=",obs_weight,
                     ",reg_weight=",reg_weight,
                     ",reg_type=\"",reg_type,
                     "\",l=",l,
                     ",extrap_rho=",extrap_rho,")"),
                    need_return = "Julia")
  }else{
    julia_assign("covariates_julia",covariates)
    julia_model <- julia_eval(paste0(model_type,
                     "(data_julia,covariates_julia,deriv,parameters,time_column_name=\"",time_column_name,
                     "\",hidden_units=",hidden_units,
                     ",seed=",seed,
                     ",proc_weight=",proc_weight,
                     ",obs_weight=",obs_weight,
                     ",reg_weight=",reg_weight,
                     ",reg_type=\"",reg_type,
                     "\",l=",l,
                     ",extrap_rho=",extrap_rho,")"),
                    need_return = "Julia")
  }
  return(julia_model)
}