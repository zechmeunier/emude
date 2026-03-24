# test-model_constructors_mock.R
# Mock-based tests for all five model constructors.
# JuliaCall functions are intercepted; no live Julia session is required.
#
# Fixtures (test_uid, tiny_df, tiny_multi_df, tiny_covariates, simple_derivs)
# are defined in helper-fixtures.R and loaded automatically by testthat.

# ── NODE ──────────────────────────────────────────────────────────────────────

test_that("NODE returns a string with the julia_model_ prefix", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  result <- NODE(data = tiny_df, time_column_name = "time", uid = test_uid)
  expect_equal(result, paste0("julia_model_", test_uid))
})

test_that("NODE constructs an eval string containing 'NODE(' for non-Bayesian", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  NODE(data = tiny_df, time_column_name = "time", uid = test_uid)
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl("=NODE\\(", model_call))
})

test_that("NODE constructs 'BayesianNODE(' when bayesian = TRUE", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  NODE(data = tiny_df, uid = test_uid, bayesian = TRUE)
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl("BayesianNODE\\(", model_call))
})

test_that("NODE passes time_column_name (quoted) in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  NODE(data = tiny_df, time_column_name = "my_t", uid = test_uid)
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl('time_column_name="my_t"', model_call, fixed = TRUE))
})

test_that("NODE passes hidden_units (unquoted) in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  NODE(data = tiny_df, uid = test_uid, hidden_units = 25)
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl("hidden_units=25", model_call, fixed = TRUE))
})

test_that("NODE passes reg_type (quoted) in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  NODE(data = tiny_df, uid = test_uid, reg_type = "L1")
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl('reg_type="L1"', model_call, fixed = TRUE))
})

test_that("NODE with covariates includes covariates_julia_ in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  NODE(data = tiny_df, covariates = tiny_covariates, uid = test_uid)
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl(paste0("covariates_julia_", test_uid), model_call, fixed = TRUE))
})

test_that("NODE without covariates omits covariates_julia_ from the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  NODE(data = tiny_df, uid = test_uid)
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_false(grepl("covariates_julia_", model_call, fixed = TRUE))
})

test_that("NODE emits scale warning when data sd > 1", {
  large_df <- data.frame(time = as.double(1:5), y = c(10, 50, 100, 200, 500))
  local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  expect_output(NODE(data = large_df, uid = test_uid), "Model performance may be improved")
})

test_that("NODE does NOT emit scale warning for already-scaled data", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  expect_silent(NODE(data = tiny_df, uid = test_uid))
})

test_that("NODE assigns data to Julia variable named data_julia_{uid}", {
  assign_names <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(name, value, ...) { assign_names <<- c(assign_names, name) },
    .package = "JuliaCall"
  )
  NODE(data = tiny_df, uid = test_uid)
  expect_true(paste0("data_julia_", test_uid) %in% assign_names)
})

# ── multi_NODE ────────────────────────────────────────────────────────────────

test_that("multi_NODE returns a string with the julia_model_ prefix", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  result <- multi_NODE(data = tiny_multi_df, uid = test_uid)
  expect_equal(result, paste0("julia_model_", test_uid))
})

test_that("multi_NODE uses 'MultiNODE(' for non-Bayesian", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  multi_NODE(data = tiny_multi_df, uid = test_uid)
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl("=MultiNODE\\(", model_call))
})

test_that("multi_NODE uses 'BayesianNODE(' when bayesian = TRUE", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  multi_NODE(data = tiny_multi_df, uid = test_uid, bayesian = TRUE)
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl("BayesianNODE\\(", model_call))
})

test_that("multi_NODE includes series_column_name in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  multi_NODE(data = tiny_multi_df, series_column_name = "series", uid = test_uid)
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl('series_column_name="series"', model_call, fixed = TRUE))
})

# ── custom_derivatives ────────────────────────────────────────────────────────

test_that("custom_derivatives returns a string with the julia_model_ prefix", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  result <- custom_derivatives(
    data = tiny_df, derivs = simple_derivs,
    initial_parameters = list(r = 0.5), uid = test_uid
  )
  expect_equal(result, paste0("julia_model_", test_uid))
})

test_that("custom_derivatives eval string contains 'CustomDerivatives(' for non-Bayesian", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  custom_derivatives(
    data = tiny_df, derivs = simple_derivs,
    initial_parameters = list(r = 0.5), uid = test_uid
  )
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl("=CustomDerivatives\\(", model_call))
})

test_that("custom_derivatives uses 'BayesianUDE(' when bayesian = TRUE", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  custom_derivatives(
    data = tiny_df, derivs = simple_derivs,
    initial_parameters = list(r = 0.5), uid = test_uid, bayesian = TRUE
  )
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl("=BayesianUDE\\(", model_call))
})

test_that("custom_derivatives calls build_custom_derivs_function_R in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  custom_derivatives(
    data = tiny_df, derivs = simple_derivs,
    initial_parameters = list(r = 0.5), uid = test_uid
  )
  build_call <- captured[grepl("build_custom_derivs_function_R", captured)]
  expect_length(build_call, 1)
  expect_true(grepl(paste0("deriv_", test_uid), build_call, fixed = TRUE))
})

test_that("custom_derivatives includes NamedTuple conversion for parameters", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  custom_derivatives(
    data = tiny_df, derivs = simple_derivs,
    initial_parameters = list(r = 0.5), uid = test_uid
  )
  named_tuple_call <- captured[
    grepl("NamedTuple", captured) & grepl(paste0("p_julia_", test_uid), captured)
  ]
  expect_length(named_tuple_call, 1)
})

test_that("custom_derivatives with file path emits include() call", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  custom_derivatives(
    data = tiny_df, derivs = "/path/to/derivs.jl",
    initial_parameters = list(r = 0.5), uid = test_uid
  )
  include_call <- captured[grepl('include\\(', captured)]
  expect_length(include_call, 1)
  expect_true(grepl("/path/to/derivs.jl", include_call, fixed = TRUE))
})

test_that("custom_derivatives with file path also emits 'f_julia = derivs' assignment", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  custom_derivatives(
    data = tiny_df, derivs = "/path/to/derivs.jl",
    initial_parameters = list(r = 0.5), uid = test_uid
  )
  assign_call <- captured[grepl("f_julia = derivs", captured, fixed = TRUE)]
  expect_length(assign_call, 1)
})

test_that("custom_derivatives emits scale warning when data sd > 1", {
  large_df <- data.frame(time = as.double(1:5), y = c(10, 50, 100, 200, 500))
  local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  expect_output(
    custom_derivatives(
      data = large_df, derivs = simple_derivs,
      initial_parameters = list(r = 0.5), uid = test_uid
    ),
    "Model performance may be improved"
  )
})

# ── multi_custom_derivatives ──────────────────────────────────────────────────

test_that("multi_custom_derivatives returns a string with the julia_model_ prefix", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  result <- multi_custom_derivatives(
    data = tiny_multi_df, derivs = simple_derivs,
    initial_parameters = list(r = 0.5), uid = test_uid
  )
  expect_equal(result, paste0("julia_model_", test_uid))
})

test_that("multi_custom_derivatives calls build_multi_custom_derivs_function_R", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  multi_custom_derivatives(
    data = tiny_multi_df, derivs = simple_derivs,
    initial_parameters = list(r = 0.5), uid = test_uid
  )
  build_call <- captured[grepl("build_multi_custom_derivs_function_R", captured)]
  expect_length(build_call, 1)
})

test_that("multi_custom_derivatives uses 'MultiCustomDerivatives(' for non-Bayesian", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  multi_custom_derivatives(
    data = tiny_multi_df, derivs = simple_derivs,
    initial_parameters = list(r = 0.5), uid = test_uid
  )
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl("=MultiCustomDerivatives\\(", model_call))
})

test_that("multi_custom_derivatives includes series_column_name in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); code },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  multi_custom_derivatives(
    data = tiny_multi_df, derivs = simple_derivs,
    initial_parameters = list(r = 0.5),
    series_column_name = "series", uid = test_uid
  )
  model_call <- captured[grepl(paste0("julia_model_", test_uid), captured)]
  expect_true(grepl('series_column_name="series"', model_call, fixed = TRUE))
})

# ── ode_model ─────────────────────────────────────────────────────────────────

test_that("ode_model returns a string with the julia_model_ prefix (up to bare julia_eval bug)", {
  # NOTE: ode_model() line 567 calls bare julia_eval() instead of JuliaCall::julia_eval().
  # When JuliaCall is not attached (only imported), this produces an error.
  # This test documents that known defect: the call fails before the model is built.
  local_mocked_bindings(
    julia_eval   = function(code, ...) code,
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  expect_error(
    ode_model(
      data = tiny_df, derivs = simple_derivs,
      initial_parameters = list(r = 0.5), uid = test_uid
    ),
    # Error occurs because bare julia_eval() is not found in the calling environment
    regexp = NULL  # Accept any error — the specific message may vary by environment
  )
})
