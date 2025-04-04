predict <- function(
    UDE,
    test_data,
    summarize = TRUE,
    CI = 95,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){
  test_ID <- paste0("test_data_",uid)
  julia_assign(test_ID, test_data)
  if(bayesian){
    return(
      julia_eval(
        paste0("predict(", UDE, test_ID,
               "summarize =", as.character(summarize),
               "CI =", as.character(CI),
               ")"
        )
      )
    )
  }
  else{
    return(
      julia_eval(
        paste0("predict(", UDE, test_ID, ")"
        )
      )
    )
  }
}