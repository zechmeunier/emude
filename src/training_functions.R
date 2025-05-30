train <- function(
    model,
    loss_function = "derivative matching",
    optimizer = "ADAM",
    regularization_weight = 0.0,
    verbose = TRUE,
    loss_options = list(),
    optim_options = list())
  
{
  verbose <- ifelse(verbose,"true","false")
  julia_assign("loss_options",loss_options)
  julia_assign("optim_options",optim_options)
  julia_eval(paste0("train!(",model,",loss_function=","\"", loss_function, "\"",
                    ",optimizer=","\"",optimizer,"\"",
                    ",regularization_weight=",regularization_weight,
                    ",verbose=",verbose,
                    ",loss_options=NamedTuple(loss_options)",
                    ",optim_options=NamedTuple(optim_options))"))
  print("Done! :)")
}
