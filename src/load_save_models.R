load_julia_model_weights <- function(model,file){
  julia_eval(paste0("load_model_parameters!(",model,",",file,")"))
}

save_julia_model_weights <- function(model,file){
  julia_eval(paste0("save_model_parameters(",model,",",file,")"))
}