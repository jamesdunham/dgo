shape <- function(item) {
  item$restrict()
  item$reweight()
  item$make_gt_variables()
  item$list_groups()
  item$list_groups_t()
  item$group_n()
  item$find_missingness()
  item$make_WT()
  item$make_l2_only()
  item$make_modifier_group_n()  
  item$make_WT()
  item$make_l2_only()
  item$make_NNl2()
  item$make_SSl2()
  item$make_group_design()
  item$make_ZZ()
  item$make_ZZ_prior()
  stan_data <- tostan(item)
  check(stan_data)
  stan_data
}

check <- function(stan_data) {
  check_dimensions(stan_data)
  check_values(stan_data)
  check_order(stan_data)
}
