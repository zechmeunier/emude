#' Calculate the column maxima
#'
#' `colmax()` calculates the maximum (highest) value per column.
#'
#' @param data A data frame with any dimensions.
#'
#' @return Vector of column maxima, with length equal to the number of columns.
#'
#' @examples
#' testdata <- data.frame("speciesA" = rpois(40,2),
#'                        "speciesB" = rpois(40,3))
#' colmax(data = testdata)
#'
#' @export
colmax <- function(data) {
  colmaxes <- as.numeric(sapply(data, function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))))
  colmaxes  }

#' Calculate the column minima
#'
#' `colmin()` calculates the minimum (lowest) value per column.
#'
#' @param data A data frame with any dimensions.
#'
#' @return Vector of column minima, with length equal to the number of columns.
#'
#' @examples
#' testdata <- data.frame("speciesA" = rpois(40,2),
#'                        "speciesB" = rpois(40,3))
#' colmax(data = testdata)
#'
#' @export
colmin <- function(data) {
  colmins <- as.numeric(sapply(data, function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))))
  colmins  }

#' Relativize data frame by its column maxima
#'
#' `rel_colmax()` relativizes a data frame by the maximum values per column.
#' It will exclude the specified time and series columns.
#'
#' @param data A data frame with any dimensions containing `time_column_name`, `series_column_name`, and observations.
#' @param time_column_name The column that contains the time data, indicating when the observations were made.
#' @param series_column_name The column that contains the series data, indicating the identifying information for the observations.
#'
#' @return A data frame scaled so that values are proportions of the maximum value per column.
#'
#' @examples
#' testdata <- data.frame("time" = rep(seq(1,10),4),
#'                        "transect" = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10)),
#'                        "speciesA" = rpois(40,2),
#'                        "speciesB" = rpois(40,3))
#' rel_colmax(testdata, time_column_name = "time", series_column_name = "transect")
#'
#' @export
rel_colmax <- function(data, time_column_name = "time", series_column_name = "series") {

  cols_to_exclude <- intersect(c(time_column_name, series_column_name), names(data))
  cols_to_rel <- setdiff(names(data), cols_to_exclude)
  data_to_rel <- data[cols_to_rel]

  colmaxes <- colmax(data_to_rel)

  data[cols_to_rel] <- sweep(data_to_rel, 2, colmaxes, `/`)

  return(data)
}

#' Relativize data frame by its column maxima and minima
#'
#' `rel_minmax()` relativizes a data frame by the maximum and minimum values per
#' column. It will exclude the specified time and series columns.
#'
#' @param data A data frame with any dimensions containing `time_column_name`, `series_column_name`, and observations.
#' @param time_column_name The column that contains the time data, indicating when the observations were made.
#' @param series_column_name The column that contains the series data, indicating the identifying information for the observations.
#'
#' @return A data frame scaled so that minimum and maximum values per column are 0 and 1, respectively.
#'
#' @examples
#' testdata <- data.frame("time" = rep(seq(1,10),4),
#'                        "transect" = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10)),
#'                        "speciesA" = rpois(40,2),
#'                        "speciesB" = rpois(40,3))
#' rel_colmax(testdata, time_column_name = "time", series_column_name = "transect")
#'
#' @export
rel_minmax <- function(data, time_column_name = "time", series_column_name = "series") {

  cols_to_exclude <- intersect(c(time_column_name, series_column_name), names(data))
  cols_to_rel <- setdiff(names(data), cols_to_exclude)
  data_to_rel <- data[cols_to_rel]

  colmins <- colmin(data_to_rel)
  colmaxes <- colmax(data_to_rel)

  data[cols_to_rel] <- sweep(
    sweep(data_to_rel, 2, colmins, `-`),
    2, colmaxes - colmins, `/`
  )

  return(data)
}

#' Split time series
#'
#' `split_series()` separates the complete time series into training data and testing data.
#' It will group the dataset based on the `split_by` column identifier, and then split
#' based on the provided training fraction, with a default of 80%/20% for training/testing.
#'
#' @param data A data frame with any dimensions containing observations, time, and series.
#' @param time_column_name The column that contains the time data, indicating when the observations were made.
#' @param series_column_name The column that contains the series data, indicating
#' the identifying information for the observations. Optional; if not found, the entire data frame is treated as one series.
#' @param split_by Split by time (default) or series. The former will use the first set of observations
#' across all series as the training data, the latter will randomly select series across all time points
#' for training data.
#' @param split_by_seed The random seed for repeating the series splits. Only necessary if `split_by = "series"`.
#' @param train_frac The proportion \[0,1\] of observations that represents data
#' used for training and validation. The testing data are calculated as the remaining proportion.
#'
#' @returns A list containing two data frames: `train` and `test`.
#' @export
#'
#' @examples
#' testdata <- data.frame("year" = rep(seq(1,10),4),
#'                        "transect" = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10)),
#'                        "speciesA" = rpois(40,2),
#'                        "speciesB" = rpois(40,3))
#' split_series(data = testdata, time_column_name = "year",
#'              series_column_name = "transect", split_by = "time")
#' split_series(data = testdata, time_column_name = "year",
#'              series_column_name = "transect", split_by = "series",
#'              split_by_seed = 2, train_frac = 0.75)
#'
split_series <- function(data,
                         time_column_name = "time",
                         series_column_name = "series",
                         split_by = "time",
                         split_by_seed = 36,
                         train_frac = 0.8) {

  # 1. Setup time column symbol and throw error if not supplied
  time_col <- rlang::sym(time_column_name)

  # 2. Arrange the data chronologically
  data_arranged <- data %>%
    dplyr::arrange({{time_col}})

  # 3. Split the observed data into training and testing sets

  # Check if the series column exists in the data frame
  has_series <- series_column_name %in% names(data)
  # If the series column exists and time is the split_by variable
  if (has_series & split_by == "time") {
    series_col <- rlang::sym(series_column_name)

    data_indexed <- data_arranged %>%
      dplyr::group_by({{series_col}}) %>%
      dplyr::mutate(
        row_num = dplyr::row_number(),
        max_row = max(row_num),
        split_index = ceiling(train_frac * max_row)
      ) %>%
      dplyr::ungroup()

    train_data <- data_indexed %>%
      dplyr::filter(row_num <= split_index) %>%
      dplyr::select(-row_num, -max_row, -split_index)

    test_data <- data_indexed %>%
      dplyr::filter(row_num > split_index) %>%
      dplyr::select(-row_num, -max_row, -split_index)
  }
  # If the series column exists and series is the split_by variable
  else if (has_series & split_by == "series") {
    series_col <- rlang::sym(series_column_name)

    series_values <- data_arranged %>%
      dplyr::pull({{series_col}}) %>%
      unique()

    set.seed(split_by_seed)
    train_series <- sample(series_values,
                           size = floor(train_frac * length(series_values)),
                           replace = FALSE)

    train_data <- data_arranged %>%
      dplyr::filter({{series_col}} %in% train_series)

    test_data <- data_arranged %>%
      dplyr::filter(!{{series_col}} %in% train_series)
  }
  # If the series column does not exist, treat as one time series
  else {
    data_indexed <- data_arranged %>%
      dplyr::mutate(
        row_num = dplyr::row_number(),
        max_row = max(row_num),
        split_index = ceiling(train_frac * max_row)
      )

    train_data <- data_indexed %>%
      dplyr::filter(row_num <= split_index) %>%
      dplyr::select(-row_num, -max_row, -split_index)

    test_data <- data_indexed %>%
      dplyr::filter(row_num > split_index) %>%
      dplyr::select(-row_num, -max_row, -split_index)
  }

  # 4. Return the list
  split_data <- list(train = train_data, test = test_data)

  return(split_data)
}
