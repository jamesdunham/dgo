library(data.table)
aggregates = foreign::read.dta('~/Dropbox (MIT)/Survey Database/DGIRT Data/GSS_long160412.dta')
aggregates[sapply(aggregates, is.factor)] <- 
  lapply(aggregates[sapply(aggregates, is.factor)], as.character)
setDT(aggregates)
aggregates[, `:=`(state = STATEAB, STATEAB = NULL,
                      n_grp = N, N = NULL,
                      s_grp = S, S = NULL,
                      female = gender, gender = NULL)]
aggregates <- as.data.frame(aggregates)
aggregates$year <- as.integer(aggregates$year)
aggregates <- aggregates[aggregates$year %in% 2006:2008, ]
aggregates$race <- replace(aggregates$race, aggregates$race == "1", "white")
aggregates$race <- replace(aggregates$race, aggregates$race == "2", "black")
aggregates$race <- replace(aggregates$race, aggregates$race == "3", "other")
aggregates$female <- replace(aggregates$female, aggregates$female == "0", "male")
aggregates$female <- replace(aggregates$female, aggregates$female == "1", "female")
dupe_rows <- duplicated(aggregates[, c("race", "year", "item", "state")])
aggregates <- aggregates[!dupe_rows, ]
aggregates <- aggregates[, c("race", "female", "year", "item", "state", "n_grp", "s_grp")]
devtools::use_data(aggregates, internal = TRUE, overwrite = TRUE)
