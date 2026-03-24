# Extracted from test-model_analysis_mock.R:161

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "emude", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
local_mocked_bindings(
    julia_eval = function(code, need_return = "Julia", ...) {
      if (identical(need_return, "R")) "DataFrame" else invisible(NULL)
    },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
rhs <- get_right_hand_side("julia_model_test")
expect_true(is.function(rhs))
expect_equal(length(formals(rhs)), 3)
