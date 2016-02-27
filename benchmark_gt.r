library(microbenchmark)

values = sample(0:9, 1000, replace = TRUE)
gt_levels = 0:9

microbenchmark(
  base =
  {
    gt_cols <- lapply(gt_levels, function(gt) {
      ifelse(values > gt, 1L, 0L)
  })},
  cpp =
  {
    gt_cols <- lapply(gt_levels, function(gt) {
      make_gt(values, gt)
  })},
  times = 1000)
