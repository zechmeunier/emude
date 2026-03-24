# Extracted from test-training_functions_mock.R:141

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "emude", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
captured <- character(0)
local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
train_UDE(model = "julia_model_test")
expect_true(grepl("verbose=false", captured[1], fixed = TRUE))
