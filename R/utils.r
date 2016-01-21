`%>%` <- magrittr::`%>%`

# Replace NA in a vector with 0
replaceNA <- function(x) {
  replace(x, is.na(x), 0)
}

# Replace NaN in a vector with NA
replaceNaN <- function(x) {
    replace(x, is.nan(x), NA)
}

# Get count of non-NA values over x
countValid <- function(x) {
    sum(!is.na(x))
}

anyValid <- function(x) {
  any(!is.na(x))
}

# Check element-wise if !is.na over x
notNA <- function(x) {
    !is.na(x)
}
