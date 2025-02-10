train <- function(julia_model,
                  loss_function = "derivative matching",
                  optimizer = "ADAM",
                  regularization_weight = 0.0,
                  verbose = TRUE){
  verbose <- ifelse(verbose,"true","false")
  julia_assign("model",julia_model)
  julia_eval(paste0("train!(model,loss_function=",loss_function,
                    ",optimizer=",optimizer,
                    ",regularization_weight=",regularization_weight,
                    ",verbose=",verbose,")"))
  return(julia_eval("model",need_return = "Julia"))
}