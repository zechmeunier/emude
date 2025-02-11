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
##   About: Relativizes matrix by its column maxima
##   Input: mat - matrix with any dimensions
##  Output: A matrix scaled so that values are a proportion of the maximum value per column
rel_colmax <- function(mat) {
  return(mat/matrix(colmax(mat), nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE))
}

# Function: rel_minmax
##   About: Relativizes matrix by its column maxima and minima
##   Input: mat - matrix with any dimensions
##  Output: A matrix scaled so that minimum and maximum values per column are 0 and 1, respectively
rel_minmax <- function(mat) {
  return((mat - matrix(colmin(mat), nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE))
         / (matrix(colmax(mat), nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE) -
              matrix(colmin(mat), nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE)))
}
