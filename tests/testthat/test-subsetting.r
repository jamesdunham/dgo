suppressMessages({
  # context("Subset to estimation periods")
  # rm(list = ls())
  #
  # d_num <- dplyr::data_frame(year = c(1991, 1991, 1992))
  # a_num <- list(use_t = 1991, time_id = "year")
  # expect_is(subset_to_estimation_periods(d_num, a_num), "data.frame")
  # expect_equal(unique(subset_to_estimation_periods(d_num, a_num)$year), 1991)
  #
  # # pass use_t as length-0 character
  # a_char <- list(use_t = "1991", time_id = "year")
  # expect_error(subset_to_estimation_periods(d_num, a_char),
  #   "is not a numeric or integer")
  #
  # a_factor <- list(use_t = as.factor("1991"), time_id = "year")
  # expect_error(subset_to_estimation_periods(d_num, a_factor),
  #   "is not a numeric or integer")
  #
  # d_factor <- dplyr::as.tbl(dplyr::data_frame(year =
  #   as.factor(c(1991, 1991, 1992))))
  # expect_error(subset_to_estimation_periods(d_factor, a_num),
  #   "is not a numeric or integer")
  #
  # expect_is(subset_to_estimation_periods(d_num, a_num), "tbl")
  #
  # context("Drop rows missing covariates")
  #
  # rm(list = ls())
  # d <- dplyr::data_frame(id = 1:4,
  #   state = as.factor(c("A", NA, "C", "D")),
  #   race = as.factor(c("A", "B", NA, "D")),
  #   year = as.factor(c("A", "B", "C", NA)))
  # arg <- list(geo_id = "state", groups = "race", time_id = "year")
  # expect_equal(drop_rows_missing_covariates(d, c("race"), arg)$id, 1)
})
