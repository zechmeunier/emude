# test-data_wrangling.R
# Tests for data wrangling utility functions: colmax, colmin, rel_colmax,
# rel_minmax, and split_series.

# ── colmax ────────────────────────────────────────────────────────────────────

test_that("colmax returns correct column maxima for basic input", {
  df <- data.frame(a = c(1, 5, 3), b = c(10, 2, 8))
  expect_equal(colmax(df), c(5, 10))
})

test_that("colmax returns NA for an all-NA column", {
  df <- data.frame(a = c(NA, NA, NA), b = c(1, 2, 3))
  result <- colmax(df)
  expect_true(is.na(result[1]))
  expect_equal(result[2], 3)
})

test_that("colmax works on a single-column data frame", {
  df <- data.frame(x = c(-5, -1, -3))
  expect_equal(colmax(df), -1)
})

test_that("colmax works on a single-row data frame", {
  df <- data.frame(a = 7, b = 2, c = 99)
  expect_equal(colmax(df), c(7, 2, 99))
})

test_that("colmax handles negative values correctly", {
  df <- data.frame(a = c(-10, -3, -7))
  expect_equal(colmax(df), -3)
})

test_that("colmax ignores NA values when computing maximum", {
  df <- data.frame(a = c(1, NA, 3))
  expect_equal(colmax(df), 3)
})

test_that("colmax returns a numeric vector", {
  df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  result <- colmax(df)
  expect_true(is.numeric(result))
})

test_that("colmax output length equals number of columns", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  expect_length(colmax(df), 3)
})

# ── colmin ────────────────────────────────────────────────────────────────────

test_that("colmin returns correct column minima for basic input", {
  df <- data.frame(a = c(1, 5, 3), b = c(10, 2, 8))
  expect_equal(colmin(df), c(1, 2))
})

test_that("colmin returns NA for an all-NA column", {
  df <- data.frame(a = c(NA, NA), b = c(5, 10))
  result <- colmin(df)
  expect_true(is.na(result[1]))
  expect_equal(result[2], 5)
})

test_that("colmin works on a single-column data frame", {
  df <- data.frame(x = c(100, 1, 50))
  expect_equal(colmin(df), 1)
})

test_that("colmin handles negative values correctly", {
  df <- data.frame(a = c(-1, -10, -3))
  expect_equal(colmin(df), -10)
})

test_that("colmin ignores NA values when computing minimum", {
  df <- data.frame(a = c(NA, 3, 1))
  expect_equal(colmin(df), 1)
})

test_that("colmin returns a numeric vector", {
  df <- data.frame(a = c(1, 2, 3))
  expect_true(is.numeric(colmin(df)))
})

test_that("colmin output length equals number of columns", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  expect_length(colmin(df), 3)
})

# ── rel_colmax ────────────────────────────────────────────────────────────────

test_that("rel_colmax scales observation columns by column maximum", {
  df <- data.frame(time = 1:3, a = c(2, 4, 6), b = c(5, 10, 15))
  result <- rel_colmax(df, time_column_name = "time", series_column_name = "series")
  expect_equal(result$a, c(2/6, 4/6, 6/6))
  expect_equal(result$b, c(5/15, 10/15, 15/15))
})

test_that("rel_colmax preserves the time column unchanged", {
  df <- data.frame(time = c(1, 2, 3), y = c(10, 20, 30))
  result <- rel_colmax(df, time_column_name = "time")
  expect_equal(result$time, c(1, 2, 3))
})

test_that("rel_colmax preserves the series column unchanged", {
  df <- data.frame(time = 1:4, series = c("A", "A", "B", "B"), y = c(2, 4, 1, 3))
  result <- rel_colmax(df, time_column_name = "time", series_column_name = "series")
  expect_equal(result$series, c("A", "A", "B", "B"))
})

test_that("rel_colmax result maximum per observation column is 1", {
  df <- data.frame(time = 1:5, a = c(1, 2, 3, 4, 5), b = c(10, 20, 30, 40, 50))
  result <- rel_colmax(df, time_column_name = "time")
  expect_equal(max(result$a), 1)
  expect_equal(max(result$b), 1)
})

test_that("rel_colmax returns a data frame with same dimensions", {
  df <- data.frame(time = 1:5, a = rnorm(5, mean = 5), b = rnorm(5, mean = 10))
  result <- rel_colmax(df, time_column_name = "time")
  expect_equal(dim(result), dim(df))
})

test_that("rel_colmax works when series column is absent from data", {
  df <- data.frame(time = 1:3, y = c(3, 6, 9))
  # series_column_name defaults to "series" which is not present — should still work
  result <- rel_colmax(df, time_column_name = "time", series_column_name = "series")
  expect_equal(result$y, c(1/3, 2/3, 1))
})

# ── rel_minmax ────────────────────────────────────────────────────────────────

test_that("rel_minmax normalizes to [0, 1]", {
  df <- data.frame(x = c(0, 50, 100))
  result <- rel_minmax(df)
  expect_true(all(result$x >= 0 & result$x <= 1))
})

test_that("rel_minmax minimum value maps to 0", {
  df <- data.frame(time = 1:5, y = c(10, 20, 30, 40, 50))
  result <- rel_minmax(df, time_column_name = "time")
  expect_equal(min(result$y), 0)
})

test_that("rel_minmax maximum value maps to 1", {
  df <- data.frame(time = 1:5, y = c(10, 20, 30, 40, 50))
  result <- rel_minmax(df, time_column_name = "time")
  expect_equal(max(result$y), 1)
})

test_that("rel_minmax correctly normalizes multiple columns", {
  df <- data.frame(time = 1:3, a = c(0, 5, 10), b = c(100, 150, 200))
  result <- rel_minmax(df, time_column_name = "time")
  expect_equal(result$a, c(0, 0.5, 1))
  expect_equal(result$b, c(0, 0.5, 1))
})

test_that("rel_minmax preserves time column unchanged", {
  df <- data.frame(time = c(1, 2, 3), y = c(5, 10, 15))
  result <- rel_minmax(df, time_column_name = "time")
  expect_equal(result$time, c(1, 2, 3))
})

test_that("rel_minmax preserves series column unchanged", {
  df <- data.frame(time = 1:4, series = c("A","A","B","B"), y = c(1,2,3,4))
  result <- rel_minmax(df, time_column_name = "time", series_column_name = "series")
  expect_equal(result$series, c("A","A","B","B"))
})

test_that("rel_minmax returns data frame with same dimensions", {
  df <- data.frame(time = 1:10, a = rnorm(10, 5), b = rnorm(10, 20))
  result <- rel_minmax(df, time_column_name = "time")
  expect_equal(dim(result), dim(df))
})

# ── split_series ──────────────────────────────────────────────────────────────

test_that("split_series train + test row counts equal total rows (no series column)", {
  df <- data.frame(time = 1:100, y = rnorm(100))
  splits <- split_series(df, train_frac = 0.8)
  expect_equal(nrow(splits$train) + nrow(splits$test), 100)
})

test_that("split_series returns a list with 'train' and 'test' elements", {
  df <- data.frame(time = 1:10, y = rnorm(10))
  splits <- split_series(df)
  expect_true(is.list(splits))
  expect_named(splits, c("train", "test"))
})

test_that("split_series training set is approximately train_frac of total", {
  df <- data.frame(time = 1:100, y = rnorm(100))
  splits <- split_series(df, train_frac = 0.8)
  expect_equal(nrow(splits$train), 80)
})

test_that("split_series test set is approximately (1 - train_frac) of total", {
  df <- data.frame(time = 1:100, y = rnorm(100))
  splits <- split_series(df, train_frac = 0.8)
  expect_equal(nrow(splits$test), 20)
})

test_that("split_series train and test sets are non-overlapping (time is unique)", {
  df <- data.frame(time = 1:100, y = rnorm(100))
  splits <- split_series(df, train_frac = 0.8)
  expect_equal(length(intersect(splits$train$time, splits$test$time)), 0)
})

test_that("split_series training set contains earlier time points than test set", {
  df <- data.frame(time = 1:100, y = rnorm(100))
  splits <- split_series(df, train_frac = 0.8)
  expect_true(max(splits$train$time) < min(splits$test$time))
})

test_that("split_series does not add extra columns to output", {
  df <- data.frame(time = 1:20, y = rnorm(20))
  splits <- split_series(df, train_frac = 0.8)
  expect_equal(names(splits$train), names(df))
  expect_equal(names(splits$test), names(df))
})

test_that("split_series respects custom time_column_name", {
  df <- data.frame(t = 1:20, y = rnorm(20))
  splits <- split_series(df, time_column_name = "t", train_frac = 0.8)
  expect_equal(nrow(splits$train) + nrow(splits$test), 20)
})

test_that("split_series handles multiple series correctly", {
  df <- data.frame(
    time   = rep(1:50, 2),
    series = c(rep("A", 50), rep("B", 50)),
    y      = rnorm(100)
  )
  splits <- split_series(df, train_frac = 0.8)
  expect_equal(nrow(splits$train) + nrow(splits$test), 100)
  # Each series of 50 rows should contribute ceiling(0.8 * 50) = 40 rows to train
  train_A <- splits$train[splits$train$series == "A", ]
  train_B <- splits$train[splits$train$series == "B", ]
  expect_equal(nrow(train_A), 40)
  expect_equal(nrow(train_B), 40)
})

test_that("split_series with train_frac = 1 puts all rows in training set", {
  df <- data.frame(time = 1:10, y = rnorm(10))
  splits <- split_series(df, train_frac = 1)
  expect_equal(nrow(splits$train), 10)
  expect_equal(nrow(splits$test), 0)
})

test_that("split_series sorts data chronologically before splitting", {
  df <- data.frame(time = c(10, 3, 7, 1, 5), y = rnorm(5))
  splits <- split_series(df, train_frac = 0.6)
  # With 5 rows and 0.6 fraction, ceiling(0.6 * 5) = 3 rows in train
  # Sorted: 1,3,5,7,10 → train = 1,3,5; test = 7,10
  expect_true(all(splits$train$time <= splits$test$time[1]))
})
