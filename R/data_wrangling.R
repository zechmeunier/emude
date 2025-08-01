#' Calculate the column maxima
#'
#' `colmax()` calculates the maximum (highest) value per column.
#'
#' @param mat A matrix with any dimensions
#'
#' @return Vector of column maxima, with length equal to the number of columns
#'
#' @examples colmax(mat = X)
#'
#' @export
colmax <- function(mat) {
  colmaxes <- as.numeric(sapply(mat, function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))))
  colmaxes  }

#' Calculate the column minima
#'
#' `colmin()` calculates the minimum (lowest) value per column.
#'
#' @param mat A matrix with any dimensions
#'
#' @return Vector of column minima, with length equal to the number of columns
#'
#' @examples colmin(mat = X)
#'
#' @export
colmin <- function(mat) {
  colmins <- as.numeric(sapply(mat, function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))))
  colmins  }

#' Relativize matrix by its column maxima
#'
#' `rel_colmax()` relativizes a matrix by the maximum values per column. It will exclude the specified time and series columns.
#'
#' @param time_column_name The column that contains the time data, indicating when the observations were made
#' @param series_column_name The column that contains the series data, indicating the identifying information for the observations
#' @param mat A matrix with any dimensions containing `time_column_name`, `series_column_name`, and observations.
#'
#' @return A matrix scaled so that values are proportions of the maximum value per column
#' @export
rel_colmax <- function(mat, time_column_name = "time", series_column_name = "series") {

  cols_to_exclude <- intersect(c(time_column_name, series_column_name), names(mat))
  cols_to_rel <- setdiff(names(mat), cols_to_exclude)
  mat_to_rel <- mat[cols_to_rel]

  colmaxes <- colmax(mat_to_rel)

  mat[cols_to_rel] <- sweep(mat_to_rel, 2, colmaxes, `/`)

  return(mat)
}

#' Relativize matrix by its column maxima and minima
#'
#' `rel_minmax()` relativizes a matrix by the maximum and minimum values per column. It will exclude the specified time and series columns.
#'
#' @param time_column_name The column that contains the time data, indicating when the observations were made
#' @param series_column_name The column that contains the series data, indicating the identifying information for the observations
#' @param mat A matrix with any dimensions containing `time_column_name`, `series_column_name`, and observations.
#'
#' @return A matrix scaled so that minimum and maximum values per column are 0 and 1, respectively
#' @export
rel_minmax <- function(mat, time_column_name = "time", series_column_name = "series") {

  cols_to_exclude <- intersect(c(time_column_name, series_column_name), names(mat))
  cols_to_rel <- setdiff(names(mat), cols_to_exclude)
  mat_to_rel <- mat[cols_to_rel]

  colmins <- colmin(mat_to_rel)
  colmaxes <- colmax(mat_to_rel)

  mat[cols_to_rel] <- sweep(
    sweep(mat_to_rel, 2, colmins, `-`),
    2, colmaxes - colmins, `/`
  )

  return(mat)
}
