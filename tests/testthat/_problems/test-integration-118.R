# Extracted from test-integration.R:118

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
    train_UDE(m, loss_function = "derivative matching",
              optim_options = list(maxiter = 3))
  )
