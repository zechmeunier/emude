# Function: colmax
##   About: Calculates the column maxima
##   Input: mat - matrix with any dimensions
##  Output: colmaxes - vector of column maxima
colmax <- function(mat) {
  colmaxes <- as.numeric(sapply(mat, function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))))
  colmaxes  }

# Function: colmin
##   About: Calculates the column minima
##   Input: mat - matrix with any dimensions
##  Output: colmins - vector of column minima
colmin <- function(mat) {
  colmins <- as.numeric(sapply(mat, function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))))
  colmins  }

# Function: rel_colmax
##   About: Relativizes matrix by its column maxima, excluding time and series columns
##   Input: mat - matrix with any dimensions
##  Output: A matrix scaled so that values are proportions of the maximum value per column
rel_colmax <- function(mat, time_column_name = "time", series_column_name = "series") {
  
  cols_to_exclude <- intersect(c(time_column_name, series_column_name), names(mat))
  cols_to_rel <- setdiff(names(mat), cols_to_exclude)
  mat_to_rel <- mat[cols_to_rel]
  
  colmaxes <- colmax(mat_to_rel)
  
  mat[cols_to_rel] <- sweep(mat_to_rel, 2, colmaxes, `/`)
  
  return(mat)
}

# Function: rel_minmax
##   About: Relativizes matrix by column minima and maxima, excluding time and series columns
##   Input: mat - matrix with any dimensions
##  Output: A matrix scaled so that minimum and maximum values per column are 0 and 1, respectively
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