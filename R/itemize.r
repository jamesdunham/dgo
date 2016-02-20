# targetize <- function(target_data,
#                       target_groups,
#                       target_prop_name,
#                       time_id) {
#
#   targets <- Targets$new(tbl = as.data.frame(target_data))
#
#   targets$groups <- Name$new(
#       value = target_groups,
#       name = "target groups",
#       length_min = 1L,
#       superset = colnames(targets_data))
#   targets$groups$test_length()
#   targets$groups$test_membership()
#
#   targets$prop_name <- Name$new(
#       value = prop_name,
#       name = "proportion",
#       length_min = 1L,
#       length_max = 1L,
#       superset = colnames(targets_data))
#   targets$prop_name$test_length()
#   targets$prop_name$test_membership()
#
#   targets
# }

itemize <- function(arg) {
  item <- Item$new()
  item$tbl <- arg$level1
  item$items <- new("ItemVar", arg$items)
  item$groups <- new("ItemVar", arg$groups)
  item$geo <- new("ItemVar", arg$geo_id)
  item$time <- new("ItemVar", arg$time_id)
  item$survey <- new("ItemVar", arg$survey_id)
  item$weight <- new("ItemVar", arg$survey_weight) 
  item
} 
