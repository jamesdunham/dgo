rm(list=ls())
devtools::load_all('~/projects/dgirt')
data(states)
data(state_targets)
data = list(level1 = states)
vars = list(
  items = grep("^Q_", colnames(states), value = TRUE),
  groups = c("D_gender", "D_race_new"),
  time_id = "D_year",
  geo_id = "D_abb",
  survey_id = "D_source",
  survey_weight = "D_weight")

states_fmt = format_data(data, vars)
dgirt_cmd_out = run_dgirt(states_fmt, n_chain = 1, n_iter = 10, method = "optimize")
dgirt_cmd_out

group_means = dgirt_cmd_out$theta_bar %>%
  dplyr::rename(D_gender = group_1, D_race_new = group_2, year = t)

variables = c("year", "D_gender", "D_race_new")
for (v in variables) {
  group_means[[v]] = as.integer(gsub(paste0("^", v), "", group_means[[v]]))
}

group_means$D_race_new = factor(group_means$D_race_new,
  labels = levels(state_targets$D_race_new))
group_means$D_gender = factor(group_means$D_gender,
  labels = levels(state_targets$D_gender))

group_means
state_targets

means = poststratify(group_means, state_targets, variables, "Prop")
head(means)
