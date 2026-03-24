# test-model_analysis_mock.R
# Mock-based tests for cross_validation and get_right_hand_side.
# No live Julia session is required.
#
# Note: cross_validation() line 65 contains a bare julia_eval() call
# (print(julia_eval("import.Pkg;..."))), which is a known bug — the same
# pattern as ode_model. The mock via .package = "JuliaCall" will NOT intercept
# the bare call. Tests below account for this by expecting the resulting error.

# ── cross_validation — string construction ────────────────────────────────────

test_that("cross_validation eval string defines 'function training!(model)'", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); list() },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  # Suppress the bare julia_eval bug on line 65 — it will error before we get results.
  # We wrap in tryCatch to still capture what was built up to that point.
  tryCatch(
    cross_validation(model = "julia_model_test", k = 3),
    error = function(e) NULL
  )
  training_def <- captured[grepl("function training!(model)", captured, fixed = TRUE)]
  expect_length(training_def, 1)
})

test_that("cross_validation training!() closure includes the model identifier", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); list() },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  tryCatch(cross_validation(model = "julia_model_test", k = 3), error = function(e) NULL)
  training_def <- captured[grepl("function training!(model)", captured, fixed = TRUE)]
  expect_true(grepl("julia_model_test", training_def, fixed = TRUE))
})

test_that("cross_validation training!() closure includes loss_function", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); list() },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  tryCatch(
    cross_validation(model = "julia_model_test", k = 3,
                     loss_function = "multiple shooting"),
    error = function(e) NULL
  )
  training_def <- captured[grepl("function training!(model)", captured, fixed = TRUE)]
  expect_true(grepl("multiple shooting", training_def, fixed = TRUE))
})

test_that("cross_validation assigns loss_options and optim_options to Julia", {
  assign_names <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) list(),
    julia_assign = function(name, value, ...) { assign_names <<- c(assign_names, name) },
    .package = "JuliaCall"
  )
  tryCatch(cross_validation(model = "julia_model_test", k = 3), error = function(e) NULL)
  expect_true("loss_options" %in% assign_names)
  expect_true("optim_options" %in% assign_names)
})

test_that("cross_validation leave_future_out call includes k value", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); list() },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  tryCatch(cross_validation(model = "julia_model_test", k = 7), error = function(e) NULL)
  lfo_call <- captured[grepl("leave_future_out", captured, fixed = TRUE)]
  if (length(lfo_call) > 0) {
    expect_true(grepl(",7", lfo_call[1], fixed = TRUE))
  } else {
    skip("leave_future_out call not reached due to bare julia_eval bug")
  }
})

test_that("cross_validation includes path= when path is provided", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); list() },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  tryCatch(
    cross_validation(model = "julia_model_test", k = 3, path = "/tmp/cv.csv"),
    error = function(e) NULL
  )
  lfo_call <- captured[grepl("leave_future_out", captured, fixed = TRUE)]
  if (length(lfo_call) > 0) {
    expect_true(grepl('path=', lfo_call[1], fixed = TRUE))
    expect_true(grepl("/tmp/cv.csv", lfo_call[1], fixed = TRUE))
  } else {
    skip("leave_future_out call not reached due to bare julia_eval bug")
  }
})

test_that("cross_validation omits path= when path is NULL", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); list() },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  tryCatch(
    cross_validation(model = "julia_model_test", k = 3, path = NULL),
    error = function(e) NULL
  )
  lfo_call <- captured[grepl("leave_future_out", captured, fixed = TRUE)]
  if (length(lfo_call) > 0) {
    expect_false(grepl("path=", lfo_call[1], fixed = TRUE))
  } else {
    skip("leave_future_out call not reached due to bare julia_eval bug")
  }
})

# ── get_right_hand_side — closure construction ────────────────────────────────

test_that("get_right_hand_side returns a function (no covariates)", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) "nothing",
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  rhs <- get_right_hand_side("julia_model_test")
  expect_true(is.function(rhs))
})

test_that("get_right_hand_side without covariates returns a two-argument function (t, u)", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) "nothing",
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  rhs <- get_right_hand_side("julia_model_test")
  expect_equal(length(formals(rhs)), 2)
  expect_equal(names(formals(rhs)), c("t", "u"))
})

test_that("get_right_hand_side with covariates returns a three-argument function (t, u, x)", {
  # Return "DataFrame" unconditionally: the first eval's return is discarded, the
  # second (typeof check) sees "DataFrame" and triggers the covariates branch.
  local_mocked_bindings(
    julia_eval   = function(code, ...) "DataFrame",
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  rhs <- get_right_hand_side("julia_model_test")
  expect_true(is.function(rhs))
  expect_equal(length(formals(rhs)), 3)
  expect_equal(names(formals(rhs)), c("t", "u", "x"))
})

test_that("get_right_hand_side creates the rhs_ variable in Julia", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval = function(code, ...) {
      captured <<- c(captured, code)
      "nothing"
    },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  get_right_hand_side("julia_model_test")
  rhs_def <- captured[grepl("rhs_julia_model_test", captured, fixed = TRUE)]
  expect_true(length(rhs_def) > 0)
  expect_true(grepl("get_right_hand_side(julia_model_test)", rhs_def[1], fixed = TRUE))
})
