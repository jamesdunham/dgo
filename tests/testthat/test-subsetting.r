context("Subset to estimation periods")

rm(list = ls())
d_factor <- dplyr::data_frame(year = as.factor(c(1991, 1991, 1992)))
a_char <- list(use_t = c("1991"), time_id = "year")
expect_equal(levels(subset_to_estimation_periods(d_factor, a_char)$year),
  "1991")
expect_is(subset_to_estimation_periods(d_factor, a_char), "data.frame")

a_length0 <- list(use_t = character(), time_id = "year")
expect_error(subset_to_estimation_periods(d_factor, a_length0),
  "nrow.* is not TRUE")

a_str_length0 <- list(use_t = "", time_id = "year")
expect_error(subset_to_estimation_periods(d_factor, a_str_length0),
  "nrow.* is not TRUE")

a_length2 <- list(use_t = c("1991", "1992"), time_id = "year")
expect_equal(levels(subset_to_estimation_periods(d_factor, a_length2)$year),
  c("1991", "1992"))

d_num <- data.frame(year = c(1991, 1991, 1992))
expect_error(subset_to_estimation_periods(d_num, a_char),
  "factor.* is not TRUE")

d_char <- dplyr::data_frame(year = c("1991", "1991", "1992"))
expect_error(subset_to_estimation_periods(d_char, a_char),
  "factor.* is not TRUE")

a_num <- list(use_t = c(1991), time_id = "year")
expect_error(subset_to_estimation_periods(d_factor, a_num),
  "character.* is not TRUE")

a_factor <- list(use_t = as.factor("1991"))
expect_error(subset_to_estimation_periods(d_factor, a_num),
  "character.* is not TRUE")

d_tbl <- dplyr::as.tbl(dplyr::data_frame(year =
  as.factor(c(1991, 1991, 1992))))
expect_is(subset_to_estimation_periods(d_tbl, a_char), "tbl")

context("Drop rows missing covariates")
rm(list = ls())
d <- dplyr::data_frame(id = 1:4,
  state = as.factor(c("A", NA, "C", "D")),
  race = as.factor(c("A", "B", NA, "D")),
  year = as.factor(c("A", "B", "C", NA)))
arg <- list(geo_id = "state", demo_id = "race", time_id = "year")
expect_equal(drop_rows_missing_covariates(d, arg)$id, 1)
