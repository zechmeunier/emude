# test-integration.R
# End-to-end integration tests requiring a live Julia + UniversalDiffEq session.
# These tests are skipped on CRAN, CI, and any environment without Julia.
#
# When running locally with devtools::test(), julia_setup() and emude_setup()
# are called in tests/testthat.R before test_check(), so Julia is available.
# Use devtools::test(filter = "integration") to run only these tests.

skip_on_cran()
skip_on_ci()
if (!julia_is_available()) skip("Julia session not available")

# ── Shared fixture ────────────────────────────────────────────────────────────
# 25-row two-species dataset with values in [0,1] (sd < 1 → no scale warning).
# 25 rows ensures the derivative matching spline smoother has enough data for
# its default degrees-of-freedom setting (RegularizationSmooth requires n > d).
# d = 3L is passed explicitly as an R integer so JuliaCall sends it as Int64.
# maxiter = 3L keeps training time minimal.

lv_df <- data.frame(
  time = as.double(1:25),
  prey = c(0.45, 0.52, 0.60, 0.55, 0.48, 0.44, 0.50, 0.57, 0.53, 0.49,
           0.47, 0.54, 0.61, 0.56, 0.50, 0.46, 0.51, 0.58, 0.54, 0.48,
           0.43, 0.49, 0.55, 0.60, 0.52),
  pred = c(0.22, 0.25, 0.30, 0.28, 0.24, 0.21, 0.26, 0.29, 0.27, 0.23,
           0.20, 0.24, 0.31, 0.29, 0.25, 0.22, 0.27, 0.30, 0.28, 0.24,
           0.19, 0.23, 0.28, 0.31, 0.26)
)

# Shared training options: explicit d prevents RegularizationSmooth dimension
# errors; maxiter keeps Julia training time minimal.
train_opts <- list(
  loss_function = "derivative matching",
  loss_options  = list(d = 3L),
  optim_options = list(maxiter = 3L)
)

# ── NODE pipeline ─────────────────────────────────────────────────────────────

test_that("NODE returns a character string starting with 'julia_model_'", {
  m <- NODE(data = lv_df, time_column_name = "time",
            hidden_units = 5, seed = 1)
  expect_true(is.character(m))
  expect_true(startsWith(m, "julia_model_"))
})

test_that("train_UDE completes without error on a NODE model (derivative matching, 3 iter)", {
  m <- NODE(data = lv_df, time_column_name = "time",
            hidden_units = 5, seed = 1)
  expect_no_error(
    do.call(train_UDE, c(list(model = m), train_opts))
  )
})

test_that("predict_UDE returns a data frame with same columns as test_data", {
  m <- NODE(data = lv_df, time_column_name = "time",
            hidden_units = 5, seed = 1)
  do.call(train_UDE, c(list(model = m), train_opts))
  preds <- predict_UDE(UDE = m, test_data = lv_df)
  expect_true(is.data.frame(preds))
  expect_equal(names(preds), names(lv_df))
})

test_that("predict_UDE returns same number of rows as test_data", {
  m <- NODE(data = lv_df, time_column_name = "time",
            hidden_units = 5, seed = 1)
  do.call(train_UDE, c(list(model = m), train_opts))
  preds <- predict_UDE(UDE = m, test_data = lv_df)
  expect_equal(nrow(preds), nrow(lv_df) - 1L)
})

# ── get_right_hand_side ───────────────────────────────────────────────────────

test_that("get_right_hand_side returns a callable R function after training", {
  m <- NODE(data = lv_df, time_column_name = "time",
            hidden_units = 5, seed = 1)
  do.call(train_UDE, c(list(model = m), train_opts))
  rhs <- get_right_hand_side(m)
  expect_true(is.function(rhs))
})

test_that("get_right_hand_side closure returns a numeric vector of length 2", {
  m <- NODE(data = lv_df, time_column_name = "time",
            hidden_units = 5, seed = 1)
  do.call(train_UDE, c(list(model = m), train_opts))
  rhs    <- get_right_hand_side(m)
  result <- rhs(t = 1.0, u = c(0.5, 0.25))
  expect_true(is.numeric(result))
  expect_length(result, 2)
})

# ── custom_derivatives pipeline ───────────────────────────────────────────────

test_that("custom_derivatives builds a model string without error", {
  derivs_lv <- function(u, nn, p, t) {
    du1 <- p$r * u[1] - nn[1]
    du2 <- p$theta * nn[1] - p$m * u[2]
    c(du1, du2)
  }
  m <- custom_derivatives(
    data               = lv_df,
    derivs             = derivs_lv,
    initial_parameters = list(r = 0.5, theta = 0.1, m = 0.2),
    time_column_name   = "time",
    hidden_units       = 5
  )
  expect_true(startsWith(m, "julia_model_"))
})

test_that("custom_derivatives model trains without error (3 iter, derivative matching)", {
  derivs_lv <- function(u, nn, p, t) {
    du1 <- p$r * u[1] - nn[1]
    du2 <- p$theta * nn[1] - p$m * u[2]
    c(du1, du2)
  }
  m <- custom_derivatives(
    data               = lv_df,
    derivs             = derivs_lv,
    initial_parameters = list(r = 0.5, theta = 0.1, m = 0.2),
    time_column_name   = "time",
    hidden_units       = 5
  )
  expect_no_error(
    do.call(train_UDE, c(list(model = m), train_opts))
  )
})

# ── save / load round-trip ────────────────────────────────────────────────────

test_that("save_julia_model_weights writes a file and load_julia_model_weights loads it", {
  # LoadSaveModels.jl is not include()d in UniversalDiffEq.jl in the installed
  # version (B7l0K), so save_model_parameters / load_model_parameters! are
  # unreachable. Skip until the upstream package includes the file.
  skip("LoadSaveModels.jl not included in installed UniversalDiffEq.jl — save/load unavailable")

  m <- NODE(data = lv_df, time_column_name = "time",
            hidden_units = 5, seed = 1)
  do.call(train_UDE, c(list(model = m), train_opts))

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(save_julia_model_weights(m, tmp))
  expect_true(file.exists(tmp))
  expect_no_error(load_julia_model_weights(m, tmp))
})

# ── multi_NODE ────────────────────────────────────────────────────────────────

test_that("multi_NODE returns a valid 'julia_model_' string for multi-series data", {
  m <- multi_NODE(data             = tiny_multi_df,
                  time_column_name  = "time",
                  series_column_name = "series",
                  hidden_units      = 5,
                  seed              = 1)
  expect_true(is.character(m))
  expect_true(startsWith(m, "julia_model_"))
})
