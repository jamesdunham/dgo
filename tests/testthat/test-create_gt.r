context("Create gt_ variables")
suppressMessages({
  # int_24_df = data.frame(item = c(2L, 4L, NA, 2L))
  # expect_message(create_gt_variables(int_24_df, "item"),
  #   "'item' is class 'integer' with 2 non-missing values: '2', '4'")
  # expect_message(create_gt_variables(int_24_df, "item"),
  #   "considered binary with failure='2' and success='4'")
  # expect_equal(create_gt_variables(int_24_df, "item")$item_gt1,
  #   c(0L, 1L, NA, 0L))
  #
  # int_123_df = data.frame(item = c(2L, 1L, NA, 3L))
  # expect_message(create_gt_variables(int_123_df, "item"),
  #   "'item' is class 'integer' with 3 non-missing values: '1', '2', '3'")
  # expect_message(create_gt_variables(int_123_df, "item"),
  #   "considered ordinal with levels '1', '2', '3'")
  # expect_equal(create_gt_variables(int_123_df, "item")$item_gt1,
  #   c(1L, 0L, NA, 1L))
  # expect_equal(create_gt_variables(int_123_df, "item")$item_gt2,
  #   c(0L, 0L, NA, 1L))
  #
  # int_111_df = data.frame(item = rep(1L, 3))
  # expect_message(tryCatch(create_gt_variables(int_111_df, "item"),
  #     error = function(e) e),
  #   "'item' is class 'integer' with 1 non-missing values: '1'")
  # expect_error(create_gt_variables(int_111_df, "item"),
  #   "no variation in item")
  #
  # ordered_01_df = data.frame(item = factor(c("0", NA, "1", "0"), ordered = TRUE))
  # expect_message(create_gt_variables(ordered_01_df, "item"),
  #   "'item' is class 'orderedfactor' with 2 non-missing values: '0', '1'")
  # expect_message(create_gt_variables(ordered_01_df, "item"),
  #   "considered binary with failure='0' and success='1'")
  # expect_equal(create_gt_variables(ordered_01_df, "item")$item_gt1,
  #   c(0L, NA, 1L, 0L))
  #
  # ordered_11_df = data.frame(item = factor(c("-1", "1", "-1", NA), ordered = TRUE))
  # expect_message(create_gt_variables(ordered_11_df, "item"),
  #   "'item' is class 'orderedfactor' with 2 non-missing values: '-1', '1'")
  # expect_message(create_gt_variables(ordered_11_df, "item"),
  #   "considered binary with failure='-1' and success='1'")
  # expect_equal(create_gt_variables(ordered_01_df, "item")$item_gt1,
  #   c(0L, NA, 1L, 0L))
  #
  # ordered_123_df = data.frame(item = factor(c("1", "3", "2", NA), ordered = TRUE))
  # expect_message(create_gt_variables(ordered_123_df, "item"),
  #   "'item' is class 'orderedfactor' with 3 non-missing values: '1', '2', '3'")
  # expect_message(create_gt_variables(ordered_123_df, "item"),
  #   "considered ordinal with levels '1', '2', '3'")
  # expect_equal(create_gt_variables(ordered_123_df, "item")$item_gt1,
  #   c(0L, 1L, 1L, NA))
  # expect_equal(create_gt_variables(ordered_123_df, "item")$item_gt2,
  #   c(0L, 1L, 0L, NA))
  #
  # # Unobserved levels in an ordered factor should be ignored
  # ordered_extra_df = data.frame(item = factor(c(4, 1), ordered = TRUE, levels = 1:4))
  # expect_equal(dim(create_gt_variables(ordered_extra_df, "item")),
  #   c(2, 1))
  # expect_message(create_gt_variables(ordered_extra_df, "item"),
  #   "'item' is class 'orderedfactor' with 2 non-missing values: '1', '4'")
  # expect_message(create_gt_variables(ordered_extra_df, "item"),
  #   "considered binary with failure='1' and success='4'")
  # expect_equal(create_gt_variables(ordered_extra_df, "item")$item_gt1,
  #   c(1L, 0L))
})
