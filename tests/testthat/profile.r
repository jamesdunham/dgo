# library(profvis)
# load("tests/testthat/integration_env1.Rdata")
# profvis({
#   d_int1 <- shape(item_data = item_data,
#                   modifier_data = state_covariates,
#                   aggregate_data = gss,
#                   aggregate_item_names = unique(gss$item),
#                   item_names = econ_items,
#                   group_names = "black_x_urban",
#                   time_name = "year",
#                   time_filter = 1936:2014,
#                   geo_name = "state",
#                   survey_name = "D_source",
#                   weight_name = "D_weight",
#                   modifier_names = "nopooling",
#                   t1_modifier_names = c("percent_evangelicals",
#                                         "percent_hispanic",
#                                         "percent_urban"),
#                   constant_item = FALSE)
# })
