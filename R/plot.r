ggplot(samples) +
  aes(x = Period, y = value, color = Gender) +
  facet_wrap(~ Country) +
  stat_summary(fun.y = "mean_sdl", fun.args = list(mult = 1.96)) +
  stat_summary(fun.y = "mean.default", geom = "line")
