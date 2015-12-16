# pidnc = read_csv(paste0(git.path, '/checks/pidnc-by-svy.csv'))
# pidnc.melt = melt(pidnc, id.vars = c('svy.id', 'year')) %>%
#   filter(grepl('p\\.', variable)) %>%
#   mutate(variable = sub('p.', '', variable, fixed = T)) %>%
#   filter(!is.nan(value))
#
# ggplot(pidnc.melt, aes(x = year, y = value, color = variable)) +
#   geom_point(shape = 1, alpha = 1/4,
#     position = position_jitter(width = .05, height = .05)) +
#   geom_smooth(size = 2, se = FALSE) +
#   ylim(c(-.1, .75)) +
#   ylab('weighted mean') +
#   scale_colour_manual(values = c('#2b8cbe', '#2ca25f', '#99d8c9', '#e34a33'))
