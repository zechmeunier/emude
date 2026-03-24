# Extracted from test-model_constructors_mock.R:120

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "emude", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
expect_no_output(NODE(data = tiny_df, uid = test_uid))
