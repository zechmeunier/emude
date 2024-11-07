NODE <- function(
  data,
  time_column_name = "time",
  hidden_units = 10,
  seed = 1,
  proc_weight = 1.0,
  obs_weight = 1.0,
  reg_weight = 10 ^ -6,
  reg_type = "L2",
  l = 0.25,
  extrap_rho = 0.0
){
  julia_assign("data_julia",data)
  julia_eval(paste("NODE(data_julia,time_column_name=\"",time_column_name,
                   "\",hidden_units=",hidden_units,
                   ",seed=",seed,
                   ",proc_weight=",proc_weight,
                   ",obs_weight=",obs_weight,
                   ",reg_weight=",reg_weight,
                   ",reg_type=\"",reg_type,
                   "\",l=",l,
                   ",extrap_rho=",extrap_rho,")",
                   sep=""))
}


custom_derivatives_jl <- function(
  data,
  file,
  time_column_name = "time",
  hidden_units = 10,
  seed = 1,
  proc_weight = 1.0,
  obs_weight = 1.0,
  reg_weight = 10 ^ -6,
  reg_type = "L2",
  l = 0.25,
  extrap_rho = 0.0
){

  julia_eval(paste0("include(\"", file, "\")"))
  julia_assign("data_julia", data)
  julia_eval(paste(
                   "CustomDerivatives(data_julia,derivs,parameters,time_column_name=\"",
                   time_column_name,
                   "\",hidden_units=",hidden_units,
                   ",seed=",seed,
                   ",proc_weight=",proc_weight,
                   ",obs_weight=",obs_weight,
                   ",reg_weight=",reg_weight,
                   ",reg_type=\"",reg_type,
                   "\",l=",l,
                   ",extrap_rho=", extrap_rho, ")",
                   sep = ""))
}