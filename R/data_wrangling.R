#' Calculate the column maxima
#'
#' `colmax()` calculates the maximum (highest) value per column.
#'
#' @param df A data frame with any dimensions.
#'
#' @return Vector of column maxima, with length equal to the number of columns.
#'
#' @examples
#' df <- data.frame("speciesA" = rpois(40,2),
#'                  "speciesB" = rpois(40,3))
#' colmax(df = df)
#'
#' @export
colmax <- function(df) {
  colmaxes <- as.numeric(sapply(df, function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))))
  colmaxes  }

#' Calculate the column minima
#'
#' `colmin()` calculates the minimum (lowest) value per column.
#'
#' @param df A data frame with any dimensions.
#'
#' @return Vector of column minima, with length equal to the number of columns.
#'
#' @examples
#' df <- data.frame("speciesA" = rpois(40,2),
#'                  "speciesB" = rpois(40,3))
#' colmax(df = df)
#'
#' @export
colmin <- function(df) {
  colmins <- as.numeric(sapply(df, function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))))
  colmins  }

#' Relativize data frame by its column maxima
#'
#' `rel_colmax()` relativizes a data frame by the maximum values per column.
#' It will exclude the specified time and series columns.
#'
#' @param df A data frame with any dimensions containing `time_column_name`, `series_column_name`, and observations.
#' @param time_column_name The column that contains the time data, indicating when the observations were made.
#' @param series_column_name The column that contains the series data, indicating the identifying information for the observations.
#'
#' @return A data frame scaled so that values are proportions of the maximum value per column.
#'
#' @examples
#' df <- data.frame("time" = rep(seq(1,10),4),
#'                  "transect" = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10)),
#'                  "speciesA" = rpois(40,2),
#'                  "speciesB" = rpois(40,3))
#' rel_colmax(df, time_column_name = "time", series_column_name = "transect")
#'
#' @export
rel_colmax <- function(df, time_column_name = "time", series_column_name = "series") {

  cols_to_exclude <- intersect(c(time_column_name, series_column_name), names(df))
  cols_to_rel <- setdiff(names(df), cols_to_exclude)
  df_to_rel <- df[cols_to_rel]

  colmaxes <- colmax(df_to_rel)

  df[cols_to_rel] <- sweep(df_to_rel, 2, colmaxes, `/`)

  return(df)
}

#' Relativize data frame by its column maxima and minima
#'
#' `rel_minmax()` relativizes a data frame by the maximum and minimum values per
#' column. It will exclude the specified time and series columns.
#'
#' @param df A data frame with any dimensions containing `time_column_name`, `series_column_name`, and observations.
#' @param time_column_name The column that contains the time data, indicating when the observations were made.
#' @param series_column_name The column that contains the series data, indicating the identifying information for the observations.
#'
#' @return A data frame scaled so that minimum and maximum values per column are 0 and 1, respectively.
#'
#' @examples
#' df <- data.frame("time" = rep(seq(1,10),4),
#'                  "transect" = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10)),
#'                  "speciesA" = rpois(40,2),
#'                  "speciesB" = rpois(40,3))
#' rel_colmax(df, time_column_name = "time", series_column_name = "transect")
#'
#' @export
rel_minmax <- function(df, time_column_name = "time", series_column_name = "series") {

  cols_to_exclude <- intersect(c(time_column_name, series_column_name), names(df))
  cols_to_rel <- setdiff(names(df), cols_to_exclude)
  df_to_rel <- df[cols_to_rel]

  colmins <- colmin(df_to_rel)
  colmaxes <- colmax(df_to_rel)

  df[cols_to_rel] <- sweep(
    sweep(df_to_rel, 2, colmins, `-`),
    2, colmaxes - colmins, `/`
  )

  return(df)
}

#' Split time series
#'
#' `split_series()` separates the complete time series into training data and testing data.
#' It will group the dataset based on the series column information, and then split
#' based on the provided training fraction, with a default of 80%/20% for training/testing.
#'
#' @param df A data frame with any dimensions containing `time_column_name` and observations. The `series_column_name` is optional; if not found, the entire data frame is treated as one series.
#' @param time_column_name The column that contains the time data, indicating when the observations were made.
#' @param series_column_name The column that contains the series data, indicating the identifying information for the observations. Optional.
#' @param train_frac The proportion \[0,1\] of observations that represents data used for training and validation. The testing data are calculated as the remaining proportion.
#'
#' @returns A list containing two data frames: `train` and `test`.
#' @export
#'
#' @examples
#' print("test")
#'
split_series <- function(df, time_column_name = "time",
                         series_column_name = "series", train_frac = 0.8) {

  # 1. Setup time column symbol and throw error if not supplied
  time_col <- rlang::sym(time_column_name)

  # 2. Arrange the data chronologically
  df_arranged <- df %>%
    dplyr::arrange({{time_col}})

  # 3. Group by series if it exists or treat as a single series

  # Check if the series column exists in the data frame
  has_series <- series_column_name %in% names(df)

  if (has_series) { # If the series column exists, apply grouping
    series_col <- rlang::sym(series_column_name)

    df_indexed <- df_arranged %>%
      dplyr::group_by({{series_col}}) %>%
      dplyr::mutate(
        row_num = dplyr::row_number(),
        max_row = max(row_num),
        split_index = ceiling(train_frac * max_row)
      ) %>%
      dplyr::ungroup()
  } else { # If the series column does NOT exist, treat as one series
    df_indexed <- df_arranged %>%
      dplyr::mutate(
        row_num = dplyr::row_number(),
        max_row = max(row_num),
        split_index = ceiling(train_frac * max_row)
      )
  }

  # 4. Create the training and testing data frames by filtering
  train_df <- df_indexed %>%
    dplyr::filter(row_num <= split_index) %>%
    dplyr::select(-row_num, -max_row, -split_index)

  test_df <- df_indexed %>%
    dplyr::filter(row_num > split_index) %>%
    dplyr::select(-row_num, -max_row, -split_index)

  # 5. Return the list
  split_df <- list(train = train_df, test = test_df)

  return(split_df)
}
