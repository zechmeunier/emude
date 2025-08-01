#' Predict UDE
#'
#' Use the specified model to make predictions on provided testing data
#'
#' @param UDE The UDE model object in the R environment with which to perform predictions.
#' @param test_data A data frame containing the testing data to use to perform predictions.
#' @param summarize Logical (`TRUE` or `FALSE`) for whether or not to receive a summarized output from the function.
#' @param CI Logical (`TRUE` or `FALSE`) for whether or not to include a confidence interval for the prediction. Only applicable to Bayesian UDEs.
#' @param bayesian Logical (`TRUE` or `FALSE`) for whether or not the UDE is a Bayesian UDE.
#' @param uid A string that serves as a unique identifier to save the testing data into Julia. It is not recommended to modify this parameter.
#' @return df A data frame of predictions.
#' @export
predict_UDE <- function(
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
