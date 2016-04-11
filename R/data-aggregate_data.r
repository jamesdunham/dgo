# gss = foreign::read.dta('~/Dropbox (MIT)/Survey Database/DGIRT DATA/GSS_long160308.dta')
# gss$race <- as.character(factor(gss$race,
#   labels = c("white", "black", "other")))
# gss_mod = gss %>%
#   dplyr::mutate(
#     state = as.character(STATEAB),
#     year = as.integer(as.character(year)),
#     item = paste0(as.character(item), "_gt0")) %>%
#   dplyr::select(race, state, year, item, N, S) %>%
#   dplyr::group_by(race, state, year, item) %>%
#   dplyr::summarize(n_grp = sum(N), s_grp = sum(S))
# dplyr::glimpse(gss_mod)
# aggregate_data <- gss_mod
# saveRDS(aggregate_data, '../aggregate_data.Rds')
# readRDS('../aggregate_data.Rds')
