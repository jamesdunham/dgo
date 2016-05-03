aggregate_data = foreign::read.dta('~/Dropbox (MIT)/Survey Database/DGIRT Data/GSS_long160412.dta')
aggregate_data[sapply(aggregate_data, is.factor)] <- 
  lapply(aggregate_data[sapply(aggregate_data, is.factor)], as.character)
setDT(aggregate_data)
aggregate_data[, `:=`(state = STATEAB, STATEAB = NULL,
                      n_grp = N, N = NULL,
                      s_grp = S, S = NULL,
                      female = gender, gender = NULL)]
aggregate_data <- as.data.frame(aggregate_data)
aggregate_data$year <- as.integer(aggregate_data$year)
aggregate_data <- aggregate_data[aggregate_data$year %in% 2006:2008, ]
aggregate_data$race <- replace(aggregate_data$race, aggregate_data$race == "1", "white")
aggregate_data$race <- replace(aggregate_data$race, aggregate_data$race == "2", "black")
aggregate_data$race <- replace(aggregate_data$race, aggregate_data$race == "3", "other")
aggregate_data$female <- replace(aggregate_data$female, aggregate_data$female == "0", "male")
aggregate_data$female <- replace(aggregate_data$female, aggregate_data$female == "1", "female")
dupe_rows <- duplicated(aggregate_data[, c("race", "year", "item", "state")])
aggregate_data <- aggregate_data[!dupe_rows, ]
aggregate_data <- aggregate_data[, c("race", "female", "year", "item", "state", "n_grp", "s_grp")]
saveRDS(aggregate_data, "~/projects/dgirt/aggregate_data.Rds")
