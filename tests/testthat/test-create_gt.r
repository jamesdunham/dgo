context("Create gt_ variables")

gt_int = create_gt_variables(data.frame(item = c(2, 4, NA, 2)), "item")
expect_equal(gt_int$item_gt1, c(0L, 1L, NA, 0L))

expect_error(create_gt_variables(data.frame(item = rep(1, 3)), "item"),
  "no variation in item")

gt_factor01 = create_gt_variables(data.frame(item = factor(c("0", NA, "1", "0"), ordered = TRUE)), "item")
expect_equal(gt_factor01$item_gt1, c(0L, NA, 1L, 0L))

gt_factor11 = create_gt_variables(data.frame(item = factor(c("-1", "1", "-1", NA),
  ordered = TRUE)), "item")
expect_equal(gt_factor11$item_gt1, c(0L, 1L, 0L, NA))

gt_extra_levels = create_gt_variables(data.frame(item = factor(c(1, 3),
  ordered = TRUE, levels = 1:4)), "item")
expect_equal(dim(gt_extra_levels), c(2, 3))
expect_equal(gt_extra_levels$item_gt1, c(0L, 1L))
expect_equal(gt_extra_levels$item_gt2, c(0L, 1L))
expect_equal(gt_extra_levels$item_gt3, c(0L, 0L))
