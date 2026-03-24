# test-load_save_models_mock.R
# Mock-based tests for save_julia_model_weights and load_julia_model_weights.
# No live Julia session is required.

# ── save_julia_model_weights ──────────────────────────────────────────────────

test_that("save_julia_model_weights calls save_model_parameters() in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval = function(code, ...) { captured <<- code; invisible(NULL) },
    .package = "JuliaCall"
  )
  save_julia_model_weights("julia_model_test", "weights.json")
  expect_true(grepl("save_model_parameters(", captured, fixed = TRUE))
})

test_that("save_julia_model_weights includes the model identifier in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval = function(code, ...) { captured <<- code; invisible(NULL) },
    .package = "JuliaCall"
  )
  save_julia_model_weights("julia_model_test", "weights.json")
  expect_true(grepl("julia_model_test", captured, fixed = TRUE))
})

test_that("save_julia_model_weights includes the file path (quoted) in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval = function(code, ...) { captured <<- code; invisible(NULL) },
    .package = "JuliaCall"
  )
  save_julia_model_weights("julia_model_test", "weights.json")
  expect_true(grepl("weights.json", captured, fixed = TRUE))
})

test_that("save_julia_model_weights returns invisibly (no visible return value)", {
  local_mocked_bindings(
    julia_eval = function(code, ...) invisible(NULL),
    .package = "JuliaCall"
  )
  result <- save_julia_model_weights("julia_model_test", "weights.json")
  # julia_eval returns invisibly here; result should be NULL or invisible
  expect_null(result)
})

# ── load_julia_model_weights ──────────────────────────────────────────────────

test_that("load_julia_model_weights calls load_model_parameters!() in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval = function(code, ...) { captured <<- code; invisible(NULL) },
    .package = "JuliaCall"
  )
  load_julia_model_weights("julia_model_test", "weights.json")
  expect_true(grepl("load_model_parameters!(", captured, fixed = TRUE))
})

test_that("load_julia_model_weights includes the model identifier in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval = function(code, ...) { captured <<- code; invisible(NULL) },
    .package = "JuliaCall"
  )
  load_julia_model_weights("julia_model_test", "weights.json")
  expect_true(grepl("julia_model_test", captured, fixed = TRUE))
})

test_that("load_julia_model_weights includes the file path (quoted) in eval string", {
  captured <- character(0)
  local_mocked_bindings(
    julia_eval = function(code, ...) { captured <<- code; invisible(NULL) },
    .package = "JuliaCall"
  )
  load_julia_model_weights("julia_model_test", "weights.json")
  expect_true(grepl("weights.json", captured, fixed = TRUE))
})

test_that("load_julia_model_weights returns invisibly (no visible return value)", {
  local_mocked_bindings(
    julia_eval = function(code, ...) invisible(NULL),
    .package = "JuliaCall"
  )
  result <- load_julia_model_weights("julia_model_test", "weights.json")
  expect_null(result)
})
