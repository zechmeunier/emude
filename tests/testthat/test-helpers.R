# test-helpers.R
# Tests for internal helper functions: R_to_Julia and convert_column_types.
# These are unexported functions accessed via :::

# ── R_to_Julia ────────────────────────────────────────────────────────────────

test_that("R_to_Julia returns a character string", {
  f <- function(u, nn, p, t) {
    du <- u * p$r
    du
  }
  result <- emude:::R_to_Julia(f)
  expect_true(is.character(result))
  expect_length(result, 1)
})

test_that("R_to_Julia converts <- to =", {
  f <- function(u, nn, p, t) {
    du <- u
    du
  }
  result <- emude:::R_to_Julia(f)
  expect_false(grepl("<-", result))
  expect_true(grepl("=", result))
})

test_that("R_to_Julia removes opening curly braces", {
  f <- function(u, nn, p, t) {
    du <- u
    du
  }
  result <- emude:::R_to_Julia(f)
  expect_false(grepl("\\{", result))
})

test_that("R_to_Julia converts closing braces to '; end'", {
  f <- function(u, nn, p, t) {
    du <- u
    du
  }
  result <- emude:::R_to_Julia(f)
  expect_true(grepl("end", result))
  expect_false(grepl("\\}", result))
})

test_that("R_to_Julia converts $ to . for list access", {
  f <- function(u, nn, p, t) {
    du <- u * p$r
    du
  }
  result <- emude:::R_to_Julia(f)
  expect_false(grepl("\\$", result))
  expect_true(grepl("p\\.r", result))
})

test_that("R_to_Julia converts c() to [] vector syntax", {
  f <- function(u, nn, p, t) {
    du <- c(u, 1)
    du
  }
  result <- emude:::R_to_Julia(f)
  expect_false(grepl("c\\(", result))
  expect_true(grepl("\\[", result))
})

test_that("R_to_Julia broadcasts multiplication with .*", {
  f <- function(u, nn, p, t) {
    du <- u * 2
    du
  }
  result <- emude:::R_to_Julia(f)
  expect_true(grepl("\\.", result))
})

test_that("R_to_Julia converts else if to elseif", {
  f <- function(u, nn, p, t) {
    if (u > 1) {
      du <- u
    } else if (u < 0) {
      du <- -u
    } else {
      du <- 0
    }
    du
  }
  result <- emude:::R_to_Julia(f)
  expect_true(grepl("elseif", result))
  expect_false(grepl("else if", result))
})

test_that("R_to_Julia produces a collapsed single-line string", {
  f <- function(u, nn, p, t) {
    du <- u
    du
  }
  result <- emude:::R_to_Julia(f)
  expect_false(grepl("\n", result))
})

# ── convert_column_types ──────────────────────────────────────────────────────

test_that("convert_column_types converts integer columns to double", {
  df <- data.frame(a = 1L, b = 2L)
  result <- emude:::convert_column_types(df)
  expect_true(is.double(result$a))
  expect_true(is.double(result$b))
})

test_that("convert_column_types leaves character columns unchanged", {
  df <- data.frame(x = c("foo", "bar"), stringsAsFactors = FALSE)
  result <- emude:::convert_column_types(df)
  expect_true(is.character(result$x))
})

test_that("convert_column_types leaves factor columns unchanged", {
  df <- data.frame(x = factor(c("a", "b")))
  result <- emude:::convert_column_types(df)
  expect_true(is.factor(result$x))
})

test_that("convert_column_types returns a data frame", {
  df <- data.frame(a = 1:3, b = c(1.1, 2.2, 3.3))
  result <- emude:::convert_column_types(df)
  expect_true(is.data.frame(result))
})

test_that("convert_column_types preserves number of rows and columns", {
  df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  result <- emude:::convert_column_types(df)
  expect_equal(dim(result), dim(df))
})

test_that("convert_column_types keeps already-double columns as double", {
  df <- data.frame(a = c(1.1, 2.2, 3.3))
  result <- emude:::convert_column_types(df)
  expect_true(is.double(result$a))
})

test_that("convert_column_types handles mixed column types in one data frame", {
  df <- data.frame(
    int_col = 1L,
    dbl_col = 1.5,
    chr_col = "a",
    stringsAsFactors = FALSE
  )
  result <- emude:::convert_column_types(df)
  expect_true(is.double(result$int_col))
  expect_true(is.double(result$dbl_col))
  expect_true(is.character(result$chr_col))
})