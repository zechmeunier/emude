#' Calculate the column maxima
#'
#' `colmax()` calculates the maximum (highest) value per column.
#'
#' @param df A data frame with any dimensions.
#'
#' @return Vector of column maxima, with length equal to the number of columns.
#'
#' @examples
#' X <- data.frame("speciesA" = rpois(40,2),
#'                 "speciesB" = rpois(40,3))
#' colmax(df = X)
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
#' X <- data.frame("speciesA" = rpois(40,2),
#'                 "speciesB" = rpois(40,3))
#' colmax(df = X)
#'
#' @export
colmin <- function(df) {
  colmins <- as.numeric(sapply(df, function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))))
  colmins  }

#' Relativize data frame by its column maxima
#'
#' `rel_colmax()` relativizes a data frame by the maximum values per column. It will exclude the specified time and series columns.
#'
#' @param time_column_name The column that contains the time data, indicating when the observations were made.
#' @param series_column_name The column that contains the series data, indicating the identifying information for the observations.
#' @param df A data frame with any dimensions containing `time_column_name`, `series_column_name`, and observations.
#'
#' @return A data frame scaled so that values are proportions of the maximum value per column.
#'
#' @examples
#' X <- data.frame("time" = rep(seq(1,10),4),
#'                 "transect" = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10)),
#'                 "speciesA" = rpois(40,2),
#'                 "speciesB" = rpois(40,3))
#' rel_colmax(X, time_column_name = "time", series_column_name = "transect")
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
#' `rel_minmax()` relativizes a data frame by the maximum and minimum values per column. It will exclude the specified time and series columns.
#'
#' @param time_column_name The column that contains the time data, indicating when the observations were made.
#' @param series_column_name The column that contains the series data, indicating the identifying information for the observations.
#' @param df A data frame with any dimensions containing `time_column_name`, `series_column_name`, and observations.
#'
#' @return A data frame scaled so that minimum and maximum values per column are 0 and 1, respectively.
#'
#' @examples
#' X <- data.frame("time" = rep(seq(1,10),4),
#'                 "transect" = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10)),
#'                 "speciesA" = rpois(40,2),
#'                 "speciesB" = rpois(40,3))
#' rel_colmax(X, time_column_name = "time", series_column_name = "transect")
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
