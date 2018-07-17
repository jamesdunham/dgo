Ctrl <- setClass("Ctrl", slots = c(item_data="data.frame", item_names = "ANY",
    time_name = "character", geo_name = "character", group_names = "ANY",
    weight_name = "ANY", time_filter='numeric'))

ctrl = Ctrl(item_data  = opinion, item_names = 'abortion', time_name = 'year',
  geo_name = 'state', group_names = 'race3', weight_name = 'weight', time_filter
  = unique(opinion$year))


data(opinion)
setDT(opinion)
item_data <- dichotomize(opinion, ctrl)
make_group_counts(item_data, NULL, ctrl)


test_data = data.frame(
  abortion = c(0, 0, 1, 2),
  year = 2018,
  state = 'MA',
  race3 = 1,
  weight = 1)

ctrl = Ctrl(item_data  = opinion, item_names = 'abortion', time_name = 'year',
  geo_name = 'state', group_names = 'race3', weight_name = 'weight',
  time_filter = unique(test_data$year))

setDT(test_data)
item_data <- dichotomize(test_data, ctrl)
make_group_counts(item_data, NULL, ctrl)[]

