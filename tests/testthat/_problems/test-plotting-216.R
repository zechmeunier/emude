# Extracted from test-plotting.R:216

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "emude", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
df <- data.frame(
    time    = rep(1:5, 2),
    species = c(rep("prey", 5), rep("predator", 5)),
    density = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  )
p <- phase_plane_2D(
    observations = df,
    names        = species,
    values       = density,
    x            = prey,
    y            = predator,
    model        = NULL,
    vectors      = FALSE
  )
