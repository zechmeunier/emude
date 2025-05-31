predict <- function(
    UDE,
    test_data,
    summarize = TRUE,
    CI = 95,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){
  test_ID <- paste0("test_data_",uid)
  julia_assign(test_ID, convert_column_types(test_data))
  if(bayesian){
    return(
      julia_eval(
        paste0("predict(", UDE, ",", test_ID, "," ,
               "summarize =", as.character(summarize), ",",
               "CI =", as.character(CI),
               ")"
        )
      )
    )
  }
  else{
    df <- as.data.frame(
      julia_eval(paste0("UniversalDiffEq.predict(", UDE, ",", test_ID, ")"))
    )
    
    # Set column names to match test_data
    colnames(df) <- names(test_data)
    return(df)
  }
}