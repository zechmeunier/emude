#' Calculate the column maxima
#'
#' `colmax()` calculates the maximum value per column.
#'
#' @param mat A matrix with any dimensions
#'
#' @return vector of column maxima, with length equal to the number of columns
#' @export
#'
#' @examples
#' colmax(mat = X)
colmax <- function(mat) {
  colmaxes <- as.numeric(sapply(mat, function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))))
  colmaxes  }

#' Calculate the column minima
#'
#' @param mat - matrix with any dimensions
#'
#' @return colmins - vector of column minima
#' @export
colmin <- function(mat) {
  colmins <- as.numeric(sapply(mat, function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))))
  colmins  }

#' Relativize matrix by its column maxima, excluding time and series columns
#'
#' @param time_column_name
#' @param series_column_name
#' @param mat - matrix with any dimensions
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

#' Relative Minmax
#'
#' Relativizes matrix by column minima and maxima, excluding time and series columns
#'
#' @param time_column_name
#' @param series_column_name
#' @param mat - matrix with any dimensions
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
