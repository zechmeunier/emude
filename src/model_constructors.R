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
    reg_weight = 10^-6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.0,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){
  if (sd(as.matrix(data[, setdiff(names(data), time_column_name)]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
  model_type <- ifelse(bayesian,"BayesianNODE","NODE")
  julia_assign("data_julia",data)
  if(is.null(covariates)){
    julia_eval(paste0("julia_model_",uid,"=", model_type,
                      "(data_julia,time_column_name=\"",time_column_name,"\"",
                      ",hidden_units=",hidden_units,
                      ",seed=",seed,
                      ",proc_weight=",proc_weight,
                      ",obs_weight=",obs_weight,
                      ",reg_weight=",reg_weight,
                      ",reg_type=\"",reg_type,"\"",
                      ",l=",l,
                      ",extrap_rho=",extrap_rho,")"))
    
    
    
  }else{
    julia_assign("covariates_julia",covariates)
    julia_eval(paste0("julia_model_",uid,"=",model_type,
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
    extrap_rho = 0.0,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){
  if (sd(as.matrix(data[, setdiff(names(data), c(time_column_name, series_column_name))]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
  model_type <- ifelse(bayesian,"BayesianNODE","MultiNODE")
  julia_assign("data_julia",data)
  if(is.null(covariates)){
    julia_eval(paste0("julia_model_",uid,"=",model_type,
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
    julia_assign("covariates_julia",covariates)
    julia_eval(paste0("julia_model_",uid,"=",model_type,
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

custom_derivatives <- function(
    data,
    derivs,
    initial_parameters,
    covariates = NULL,
    neural_network_inputs = 1,
    neural_network_outputs = 1,
    hidden_units = 10,
    time_column_name = "time",
    proc_weight = 1.0,
    obs_weight = 1.0,
    reg_weight = 10^-6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.0,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){
  if (sd(as.matrix(data[, setdiff(names(data), time_column_name)]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
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
  julia_assign("hidden_units", hidden_units)
  
  julia_eval("deriv, parameters = build_custom_derivs_function_R(f_julia,p_julia,inputs,hidden_units,outputs)")
  
  if(is.null(covariates)){
    julia_eval(paste0("julia_model_",uid,"=",model_type,
                      "(data_julia,deriv,parameters,time_column_name=\"",time_column_name,"\"",
                      ",proc_weight=",proc_weight,
                      ",obs_weight=",obs_weight,
                      ",reg_weight=",reg_weight,
                      ",reg_type=\"",reg_type,"\"",
                      ",l=",l,
                      ",extrap_rho=",extrap_rho,")"),
               need_return = "Julia")
  }else{
    julia_assign("covariates_julia",covariates)
    julia_eval(paste0("julia_model_",uid,"=",model_type,
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

multi_custom_derivatives <- function(
    data,
    derivs,
    initial_parameters,
    covariates = NULL,
    neural_network_inputs = 1,
    neural_network_outputs = 1,
    hidden_units = 10,
    time_column_name = "time",
    series_column_name = "series",
    proc_weight = 1.0,
    obs_weight = 1.0,
    reg_weight = 10^-6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.0,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){
  if (sd(as.matrix(data[, setdiff(names(data), c(time_column_name, series_column_name))]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
  model_type <- ifelse(bayesian,"BayesianUDE","MultiCustomDerivatives")
  
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
  julia_assign("hidden_units",hidden_units)
  
  julia_eval("deriv, parameters = build_multi_custom_derivs_function_R(f_julia,p_julia,inputs,hidden_units,outputs)")
  
  if(is.null(covariates)){
    
    julia_eval(paste0("julia_model_",uid,"=",model_type,
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
    julia_assign("covariates_julia",covariates)
    julia_eval(paste0("julia_model_",uid,"=",model_type,
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

ode_model <- function(
    data,
    derivs,
    initial_parameters,
    covariates = NULL,
    time_column_name = "time",
    proc_weight = 1.0,
    obs_weight = 1.0,
    reg_weight = 10^-6,
    reg_type = "L2",
    l = 0.25,
    extrap_rho = 0.0
    #bayesian = FALSE
){
  if (sd(as.matrix(data[, setdiff(names(data), time_column_name)]), na.rm = TRUE) > 1) {
    cat("Model performance may be improved by scaling the data through transformation or relativization.",
        "Package options include relativization by column maximum (rel_colmax) and min-max normalization (rel_minmax).\n")
  }
  #model_type <- ifelse(bayesian,"BayesianUDE","CustomDerivatives")
  model_type <- "CustomDerivatives"
  
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
  
  julia_model <- julia_eval("deriv, parameters = build_custom_ode(f_julia,p_julia,inputs,outputs)")
  
  if(is.null(covariates)){
    julia_eval(paste0("julia_model_",uid,"=",model_type,
                      "(data_julia,deriv,parameters,time_column_name=\"",time_column_name,"\"",
                      ",proc_weight=",proc_weight,
                      ",obs_weight=",obs_weight,
                      ",reg_weight=",reg_weight,
                      ",reg_type=\"",reg_type,"\"",
                      ",l=",l,
                      ",extrap_rho=",extrap_rho,")"),
               need_return = "Julia")
  }else{
    julia_assign("covariates_julia",covariates)
    julia_eval(paste0("julia_model_",uid,"=",model_type,
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
