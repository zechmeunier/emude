#' Predict UDE
#'
#' Use the specified model to make predictions on provided testing data
#'
#' @param UDE the UDE model object in the R environment to perform predictions with
#' @param test_data A DataFrame with the data to use to perform predictions
#' @param summarize True or False, whether or not to receive a summarized output from the function
#' @param CI For Bayesian UDEs only: True or False, whether or not to include a confidence interval for the prediction
#' @param bayesian True or False whether or not the UDE is a Bayesian UDE
#' @param uid A string which serves as a unique identifier to save the test data into Julia. it is not recommended to modify this parameter
#' @return
#' @export
predict_ude <- function(
    UDE,
    test_data,
    summarize = TRUE,
    CI = 95,
    bayesian = FALSE,
    uid = gsub(x=format(Sys.time(), "%Y%m%d%H%M%OS6"),pattern = "[.]",replacement="")
){
  test_ID <- paste0("test_data_",uid)
  JuliaCall::julia_assign(test_ID, convert_column_types(test_data))
  if(bayesian){
    return(
      JuliaCall::julia_eval(
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
      JuliaCall::julia_eval(paste0("UniversalDiffEq.predict(", UDE, ",", test_ID, ")"))
    )

    # Set column names to match test_data
    colnames(df) <- names(test_data)
    return(df)
  }
}
