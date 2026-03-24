# Extracted from test-integration.R:40

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "emude", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
skip_on_cran()
skip_on_ci()
if (!julia_is_available()) skip("Julia session not available")
lv_df <- data.frame(
  time = as.double(1:10),
  prey = c(0.45, 0.52, 0.60, 0.55, 0.48, 0.44, 0.50, 0.57, 0.53, 0.49),
  pred = c(0.22, 0.25, 0.30, 0.28, 0.24, 0.21, 0.26, 0.29, 0.27, 0.23)
)

# test -------------------------------------------------------------------------
m <- NODE(data = lv_df, time_column_name = "time",
            hidden_units = 5, seed = 1)
expect_no_error(
    train_UDE(m,
              loss_function = "derivative matching",
              optimizer     = "Adam",
              optim_options = list(maxiter = 3))
  )
