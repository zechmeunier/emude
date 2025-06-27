cross_validation <- function(
    model,
    k = 10,
    loss_function = "derivative matching",
    optimizer = "ADAM",
    regularization_weight = 0.0,
    verbose = TRUE,
    loss_options = list(),
    optim_options = list(),
    path = "Null")
  
{
  verbose <- ifelse(verbose,"true","false")
  julia_assign("loss_options",loss_options)
  julia_assign("optim_options",optim_options)

  print(julia_eval("import.Pkg;Pkg.status(\"UniversalDiffEq\")"))
  julia_eval(paste0("cv_data=UniversalDiffEq.leave_future_out(",model,", training!, ", k,")"),
             need_return = "R")
  print("Done! :)")
  return(cv_data)
}

