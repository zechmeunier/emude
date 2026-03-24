# Extracted from test-integration.R:139

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "emude", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran()
skip_on_ci()
if (!julia_is_available()) skip("Julia session not available")
lv_df <- data.frame(
  time = as.double(1:25),
  prey = c(0.45, 0.52, 0.60, 0.55, 0.48, 0.44, 0.50, 0.57, 0.53, 0.49,
           0.47, 0.54, 0.61, 0.56, 0.50, 0.46, 0.51, 0.58, 0.54, 0.48,
           0.43, 0.49, 0.55, 0.60, 0.52),
  pred = c(0.22, 0.25, 0.30, 0.28, 0.24, 0.21, 0.26, 0.29, 0.27, 0.23,
           0.20, 0.24, 0.31, 0.29, 0.25, 0.22, 0.27, 0.30, 0.28, 0.24,
           0.19, 0.23, 0.28, 0.31, 0.26)
)
train_opts <- list(
  loss_function = "derivative matching",
  loss_options  = list(d = 3L),
  optim_options = list(maxiter = 3L)
)

# test -------------------------------------------------------------------------
m <- NODE(data = lv_df, time_column_name = "time",
            hidden_units = 5, seed = 1)
do.call(train_UDE, c(list(model = m), train_opts))
tmp <- tempfile(fileext = ".json")
on.exit(unlink(tmp), add = TRUE)
expect_no_error(save_julia_model_weights(m, tmp))
expect_true(file.exists(tmp))
