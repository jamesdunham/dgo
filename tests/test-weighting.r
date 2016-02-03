context("Rescale target proportions")

toy_data = read.table(header = TRUE, text = "
  t  group  weight  proportion
  1  A      1       .25
  1  B      1       .75
  2  A      1       .50
  2  B      1       .50")

# if the proportions already sum to 1 within our strata, they should be left unchanged
(no_change = get_group_props(toy_data, c("group", "t"), "proportion", "t"))
expect_equal(no_change$scaled_prop, rep(1, 4))

# group A in t=1 is 1/3 of the group-A stratum across all t
(group_props = get_group_props(toy_data, c("group"), "proportion", "t"))
expect_equal(group_props$scaled_prop[1], 1/3)
