# test-model_testing_mock.R
# Mock-based tests for predict_UDE.
# No live Julia session is required.

# ── Non-Bayesian path ─────────────────────────────────────────────────────────

test_that("predict_UDE non-Bayesian calls UniversalDiffEq.predict() (qualified)", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code)
                                         matrix(1:10, nrow = 5, ncol = 2) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
              bayesian = FALSE, uid = test_uid)
  expect_true(grepl("UniversalDiffEq.predict(", captured[1], fixed = TRUE))
})

test_that("predict_UDE non-Bayesian does NOT use plain predict() without namespace", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code)
                                         matrix(1:10, nrow = 5, ncol = 2) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
              bayesian = FALSE, uid = test_uid)
  # Must start with UniversalDiffEq.predict, not bare predict(
  expect_false(grepl("^predict\\(", captured[1]))
})

test_that("predict_UDE non-Bayesian returns a data frame", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) matrix(1:10, nrow = 5, ncol = 2),
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  result <- predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
                        bayesian = FALSE, uid = test_uid)
  expect_true(is.data.frame(result))
})

test_that("predict_UDE non-Bayesian result has same column names as test_data", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) matrix(1:10, nrow = 5, ncol = 2),
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  result <- predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
                        bayesian = FALSE, uid = test_uid)
  expect_equal(names(result), names(tiny_df))
})

test_that("predict_UDE non-Bayesian result has same row count as test_data", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) matrix(1:10, nrow = 5, ncol = 2),
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  result <- predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
                        bayesian = FALSE, uid = test_uid)
  expect_equal(nrow(result), nrow(tiny_df))
})

test_that("predict_UDE non-Bayesian eval string includes the model identifier", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code)
                                         matrix(1:10, nrow = 5, ncol = 2) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
              bayesian = FALSE, uid = test_uid)
  expect_true(grepl("julia_model_test", captured[1], fixed = TRUE))
})

# ── Bayesian path ─────────────────────────────────────────────────────────────

test_that("predict_UDE Bayesian uses plain 'predict(' without namespace qualifier", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); list() },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
              bayesian = TRUE, uid = test_uid)
  expect_true(grepl("^predict\\(", captured[1]))
  expect_false(grepl("UniversalDiffEq.predict", captured[1], fixed = TRUE))
})

test_that("predict_UDE Bayesian includes CI value in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); list() },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
              bayesian = TRUE, CI = 90, uid = test_uid)
  expect_true(grepl("CI =90", captured[1], fixed = TRUE))
})

test_that("predict_UDE Bayesian includes summarize value in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); list() },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
              bayesian = TRUE, summarize = FALSE, uid = test_uid)
  expect_true(grepl("summarize =FALSE", captured[1], fixed = TRUE))
})

# ── Data assignment ───────────────────────────────────────────────────────────

test_that("predict_UDE assigns test_data to Julia variable named test_data_{uid}", {
  assign_names <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) matrix(1:10, nrow = 5, ncol = 2),
    julia_assign = function(name, value, ...) { assign_names <<- c(assign_names, name) },
    .package = "JuliaCall"
  )
  predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
              bayesian = FALSE, uid = test_uid)
  expect_true(paste0("test_data_", test_uid) %in% assign_names)
})

test_that("predict_UDE eval string references the test_data variable by uid", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code)
                                         matrix(1:10, nrow = 5, ncol = 2) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  predict_UDE(UDE = "julia_model_test", test_data = tiny_df,
              bayesian = FALSE, uid = test_uid)
  expect_true(grepl(paste0("test_data_", test_uid), captured[1], fixed = TRUE))
})
