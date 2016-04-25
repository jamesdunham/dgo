devtools::load_all()
# `is_greater` compares a vector `response` to a referent `level`.
#
# We discretize item responses in dgirt to create K - 1 indicators for item
# responses with K levels. Given [0,1,1,1] we have two levels and want an
# indicator [0,1,1,1] for whether each response is greater than the first
# response level (i.e., 0). For trichotomous responses [0,1,2,2] we want an
# indicator [0,1,1,1] for "greater than the first level" and an indicator
# [0,0,1,1] for "greater than the second level." Implementations of this in pure
# R were fairly slow; a basic translation to Rcpp is 6-8 times faster.
#
# Which seems fast enough. It'd be faster to exploit what each iteration tells
# us about future results. After creating the second-level indicator [0,1,1,1]
# for the responses [0,1,2,2], we know that the first element of the third-level
# indicator must be 0 and elements 2-4 of the first-level indicator are 1.
# 
# Value: A vector with elements taking 1L for elements of `response` greater than
# `level` and 0L otherwise, or NA for NA elements.

# binary responses
response = sample(0L:1L, 1000, replace = TRUE)
response_levels = 0
microbenchmark::microbenchmark(
  base =
  {
    res_base <- lapply(response_levels, function(level) {
      ifelse(response > level, 1L, 0L)
  })},
  cpp =
  {
    # haven't even moved the iteration over response levels to Rcpp
    res_cpp <- lapply(response_levels, function(level) {
      is_greater(response, level)
  })},
  times = 1000, unit = "relative")
stopifnot(identical(res_base, res_cpp))
stopifnot(identical(unlist(res_base), response))

# many levels
response = sample(0:9, 1000, replace = TRUE)
response_levels = 0:8
microbenchmark::microbenchmark(
  base =
  {
    res_base <- lapply(response_levels, function(level) {
      ifelse(response > level, 1L, 0L)
  })},
  cpp =
  {
    res_cpp <- lapply(response_levels, function(level) {
      is_greater(response, level)
  })},
  times = 1000, unit = "relative")
stopifnot(identical(res_base, res_cpp))

# NA in should give NA out
response = sample(c(0, 1, NA), 1000, replace = TRUE)
response_levels = 0
microbenchmark::microbenchmark(
  base =
  {
    res_base <- lapply(response_levels, function(level) {
      ifelse(response > level, 1L, 0L)
  })},
  cpp =
  {
    res_cpp <- lapply(response_levels, function(level) {
      is_greater(response, level)
  })},
  times = 1000, unit = "relative")
stopifnot(identical(res_base, res_cpp))
