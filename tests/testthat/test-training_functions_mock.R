# test-training_functions_mock.R
# Mock-based and pure-R tests for train_UDE.
# No live Julia session is required.

# ── Loss function aliasing ────────────────────────────────────────────────────

test_that("train_UDE renames 'joint likelihood' to 'conditional likelihood' in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test", loss_function = "joint likelihood")
  expect_true(grepl("conditional likelihood", captured[1], fixed = TRUE))
  expect_false(grepl("joint likelihood", captured[1], fixed = TRUE))
})

test_that("train_UDE does NOT rename 'derivative matching'", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test", loss_function = "derivative matching")
  expect_true(grepl("derivative matching", captured[1], fixed = TRUE))
})

test_that("train_UDE does NOT rename 'shooting'", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test", loss_function = "shooting")
  expect_true(grepl('"shooting"', captured[1], fixed = TRUE))
})

test_that("train_UDE does NOT rename 'multiple shooting'", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test", loss_function = "multiple shooting")
  expect_true(grepl("multiple shooting", captured[1], fixed = TRUE))
})

# ── Optimizer renaming ────────────────────────────────────────────────────────

test_that("train_UDE renames 'Adam' to 'ADAM' in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test", optimizer = "Adam")
  expect_true(grepl('optimizer="ADAM"', captured[1], fixed = TRUE))
  expect_false(grepl('"Adam"', captured[1], fixed = TRUE))
})

test_that("train_UDE passes 'BFGS' through unchanged", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test", optimizer = "BFGS")
  expect_true(grepl('optimizer="BFGS"', captured[1], fixed = TRUE))
})

# ── Marginal likelihood warning ───────────────────────────────────────────────

test_that("train_UDE warns for 'marginal likelihood' when observation_error is absent", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) invisible(NULL),
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  expect_warning(
    train_UDE(model = "julia_model_test",
              loss_function = "marginal likelihood",
              loss_options  = list()),
    "No value of observation error supplied"
  )
})

test_that("train_UDE warns for 'marginal likelihood' when observation_error is NULL", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) invisible(NULL),
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  expect_warning(
    train_UDE(model = "julia_model_test",
              loss_function = "marginal likelihood",
              loss_options  = list(observation_error = NULL)),
    "No value of observation error supplied"
  )
})

test_that("train_UDE does NOT warn for 'marginal likelihood' when observation_error is numeric", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) invisible(NULL),
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  expect_no_warning(
    train_UDE(model = "julia_model_test",
              loss_function = "marginal likelihood",
              loss_options  = list(observation_error = 0.1))
  )
})

# ── Eval string contents ──────────────────────────────────────────────────────

test_that("train_UDE passes regularization_weight in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test", regularization_weight = 0.001)
  expect_true(grepl("regularization_weight=0.001", captured[1], fixed = TRUE))
})

test_that("train_UDE always sets verbose=false in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test")
  # In mock tests, julia_assign = invisible(NULL) never calls julia_eval, so
  # captured[1] is always the train!() string.
  expect_true(grepl("verbose=false", captured[1], fixed = TRUE))
})

test_that("train_UDE eval string calls train!() with the model identifier", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test")
  expect_true(grepl("train!(julia_model_test", captured[1], fixed = TRUE))
})

test_that("train_UDE wraps loss_options as NamedTuple in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test")
  expect_true(grepl("NamedTuple(loss_options)", captured[1], fixed = TRUE))
})

test_that("train_UDE wraps optim_options as NamedTuple in the eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) { captured <<- c(captured, code); invisible(NULL) },
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test")
  expect_true(grepl("NamedTuple(optim_options)", captured[1], fixed = TRUE))
})

# ── Return / side-effect behaviour ───────────────────────────────────────────

test_that("train_UDE prints 'Done! :)' on completion", {
  local_mocked_bindings(
    julia_eval   = function(code, ...) invisible(NULL),
    julia_assign = function(...) invisible(NULL),
    .package = "JuliaCall"
  )
  expect_output(train_UDE(model = "julia_model_test"), "Done! :)")
})

test_that("train_UDE assigns loss_options to Julia before calling train!", {
  assign_names <- character(0)
  local_mocked_bindings(
    julia_eval   = function(code, ...) invisible(NULL),
    julia_assign = function(name, value, ...) { assign_names <<- c(assign_names, name) },
    .package = "JuliaCall"
  )
  train_UDE(model = "julia_model_test")
  expect_true("loss_options" %in% assign_names)
  expect_true("optim_options" %in% assign_names)
})
