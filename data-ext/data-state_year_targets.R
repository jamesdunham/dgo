library(dgo)
library(dplyr)

data(targets)

annual_state_race_targets <- aggregate(proportion ~ state + year + race3, targets, sum)
devtools::use_data(annual_state_race_targets, overwrite = TRUE)

annual_state_race_targets %>%
  group_by(year) %>%
  summarize(sum = sum(proportion))
