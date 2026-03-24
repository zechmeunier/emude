# test-plotting.R
# Tests for plotting functions: series_plot and phase_plane_2D.
# These tests cover pure-R behavior (ggplot2 object construction) and do not
# require a live Julia session.

# ── series_plot ───────────────────────────────────────────────────────────────

test_that("series_plot returns a ggplot object", {
  df <- data.frame(
    time      = rep(1:10, 2),
    species   = c(rep("A", 10), rep("B", 10)),
    abundance = rpois(20, 40)
  )
  p <- series_plot(observations = df, x = time, y = abundance, group = species)
  expect_true(inherits(p, "ggplot"))
})

test_that("series_plot uses correct x and y aesthetics", {
  df <- data.frame(
    time      = 1:5,
    species   = rep("A", 5),
    abundance = c(10, 20, 30, 40, 50)
  )
  p <- series_plot(observations = df, x = time, y = abundance, group = species)
  mapped <- rlang::as_label(p$mapping$x)
  expect_equal(mapped, "time")
})

test_that("series_plot works without predictions argument", {
  df <- data.frame(
    time      = rep(1:5, 2),
    species   = c(rep("A", 5), rep("B", 5)),
    abundance = rpois(10, 20)
  )
  expect_no_error(
    series_plot(observations = df, x = time, y = abundance, group = species)
  )
})

test_that("series_plot accepts a predictions data frame", {
  obs <- data.frame(
    time      = rep(1:5, 2),
    species   = c(rep("A", 5), rep("B", 5)),
    abundance = rpois(10, 30)
  )
  preds <- data.frame(
    time      = rep(1:5, 2),
    species   = c(rep("A", 5), rep("B", 5)),
    abundance = c(seq(5, 25, 5), seq(10, 30, 5))
  )
  p <- series_plot(
    observations = obs,
    x            = time,
    y            = abundance,
    group        = species,
    predictions  = preds
  )
  expect_true(inherits(p, "ggplot"))
})

test_that("series_plot contains a geom_point layer", {
  df <- data.frame(
    time      = 1:5,
    species   = rep("A", 5),
    abundance = rpois(5, 20)
  )
  p <- series_plot(observations = df, x = time, y = abundance, group = species)
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomPoint" %in% layer_classes)
})

test_that("series_plot with predictions contains a geom_line layer", {
  obs <- data.frame(
    time      = 1:5,
    species   = rep("A", 5),
    abundance = rpois(5, 20)
  )
  preds <- obs
  p <- series_plot(
    observations = obs,
    x            = time,
    y            = abundance,
    group        = species,
    predictions  = preds
  )
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomLine" %in% layer_classes)
})

test_that("series_plot without predictions does not have a geom_line layer", {
  df <- data.frame(
    time      = 1:5,
    species   = rep("A", 5),
    abundance = rpois(5, 20)
  )
  p <- series_plot(observations = df, x = time, y = abundance, group = species)
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_false("GeomLine" %in% layer_classes)
})

test_that("series_plot works with a single species group", {
  df <- data.frame(
    time      = 1:10,
    species   = rep("A", 10),
    abundance = rpois(10, 5)
  )
  expect_no_error(
    series_plot(observations = df, x = time, y = abundance, group = species)
  )
})

test_that("series_plot works with four species groups", {
  df <- data.frame(
    time      = rep(1:10, 4),
    species   = c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 10)),
    abundance = rpois(40, 40)
  )
  p <- series_plot(observations = df, x = time, y = abundance, group = species)
  expect_true(inherits(p, "ggplot"))
})

# ── phase_plane_2D ────────────────────────────────────────────────────────────

# Skip all phase_plane_2D tests if the installed version lacks the vectors/model params.
# These parameters were added recently; older cached installs may not have them.
phase_plane_has_vectors <- "vectors" %in% names(formals(phase_plane_2D))

test_that("phase_plane_2D returns a ggplot object (vectors = FALSE)", {
  skip_if(!phase_plane_has_vectors, "Requires phase_plane_2D with vectors parameter")
  df <- data.frame(
    time    = rep(1:10, 2),
    species = c(rep("prey", 10), rep("predator", 10)),
    density = c(seq(0.2, 0.9, length.out = 10), seq(0.1, 0.5, length.out = 10))
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
  expect_true(inherits(p, "ggplot"))
})

test_that("phase_plane_2D contains a geom_point layer", {
  skip_if(!phase_plane_has_vectors, "Requires phase_plane_2D with vectors parameter")
  df <- data.frame(
    time    = rep(1:10, 2),
    species = c(rep("prey", 10), rep("predator", 10)),
    density = c(seq(0.2, 0.9, length.out = 10), seq(0.1, 0.5, length.out = 10))
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
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomPoint" %in% layer_classes)
})

test_that("phase_plane_2D adds geom_path layer when predictions supplied (vectors = FALSE)", {
  skip_if(!phase_plane_has_vectors, "Requires phase_plane_2D with vectors parameter")
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
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomPath" %in% layer_classes)
})

test_that("phase_plane_2D without predictions has no geom_path layer (vectors = FALSE)", {
  skip_if(!phase_plane_has_vectors, "Requires phase_plane_2D with vectors parameter")
  df <- data.frame(
    time    = rep(1:10, 2),
    species = c(rep("prey", 10), rep("predator", 10)),
    density = c(seq(0.2, 0.9, length.out = 10), seq(0.1, 0.5, length.out = 10))
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
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_false("GeomPath" %in% layer_classes)
})

test_that("phase_plane_2D pivots long data correctly (vectors = FALSE)", {
  skip_if(!phase_plane_has_vectors, "Requires phase_plane_2D with vectors parameter")
  # After pivot_wider, x and y columns (prey, predator) should exist in plot data
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
  # The plot data should have prey and predator columns after pivot_wider
  plot_data <- p$data
  expect_true("prey" %in% names(plot_data))
  expect_true("predator" %in% names(plot_data))
})
