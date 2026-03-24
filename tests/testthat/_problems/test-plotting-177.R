# Extracted from test-plotting.R:177

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "emude", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
df <- data.frame(
    time    = rep(1:10, 2),
    species = c(rep("prey", 10), rep("predator", 10)),
    density = c(seq(0.2, 0.9, length.out = 10), seq(0.1, 0.5, length.out = 10))
  )
preds <- df
p <- phase_plane_2D(
    observations = df,
    names        = species,
    values       = density,
    x            = prey,
    y            = predator,
    model        = NULL,
    predictions  = preds,
    vectors      = FALSE
  )
