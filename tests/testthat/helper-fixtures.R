# helper-fixtures.R
# Shared fixtures loaded automatically by testthat before all test files.
# Provides a fixed UID, minimal data frames, and a Julia availability check.

# Fixed UID — eliminates timestamp non-determinism in generated Julia variable names.
# All mock tests should pass uid = test_uid to model constructors.
test_uid <- "20260323120000000000"

# Minimal single-series data frame: sd of y is < 1, so no scale warning is emitted.
tiny_df <- data.frame(
  time = as.double(1:5),
  y    = c(0.1, 0.2, 0.15, 0.18, 0.22)
)

# Minimal multi-series data frame for multi_NODE / multi_custom_derivatives.
tiny_multi_df <- data.frame(
  time   = as.double(rep(1:5, 2)),
  series = c(rep("A", 5), rep("B", 5)),
  y      = c(0.1, 0.2, 0.15, 0.18, 0.22,
             0.3, 0.25, 0.28, 0.31, 0.27)
)

# Minimal covariates data frame (same time points as tiny_df).
tiny_covariates <- data.frame(
  time = as.double(1:5),
  temp = c(0.5, 0.6, 0.55, 0.58, 0.62)
)

# A simple R derivatives function used across constructor mock tests.
simple_derivs <- function(u, nn, p, t) {
  du <- p$r * u[1] - nn[1]
  c(du)
}

# Check whether a live Julia session is available.
# Used by test-integration.R to skip tests when Julia is not set up.
julia_is_available <- function() {
  tryCatch({
    JuliaCall::julia_eval("true")
    TRUE
  }, error = function(e) FALSE)
}
