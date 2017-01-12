original <- readRDS('~/Downloads/state_covariates_dgo.rds')
original$state = as.character(original$state)
original$state_name = as.character(original$state_name)
original$region = as.character(original$region)
original$fips = type.convert(original$fips)
states = original
devtools::use_data(states, overwrite = TRUE)
