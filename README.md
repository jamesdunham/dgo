dgirt is an R package for dynamic group-level IRT models as developed in
[Caughey and Warshaw 2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html):

> Over the past eight decades, millions of people have been surveyed on their
> political opinions. Until recently, however, polls rarely included enough
> questions in a given domain to apply scaling techniques such as IRT models at
> the individual level, preventing scholars from taking full advantage of
> historical survey data. To address this problem, we develop a Bayesian
> group-level IRT approach that models latent traits at the level of demographic
> and/or geographic groups rather than individuals. We use a hierarchical model
> to borrow strength cross-sectionally and dynamic linear models to do so across
> time. The group-level estimates can be weighted to generate estimates for
> geographic units. This framework opens up vast new areas of research on
> historical public opinion, especially at the subnational level.

# Quick start

```{r}
devtools::install_github("jamesdunham/dgirt", dependencies = TRUE)
library(dgirt)

# shape item response data for modeling
dgirt_in <- shape(state_opinion, item_names = "Q_cces2006_minimumwage",
                  time_name = "year", geo_name = "state", group_names = "race",
                  time_filter = 2006:2008, geo_filter = c("MA", "NY"),
                  survey_name = "source", weight_name = "weight")

# inspect data
summary(dgirt_in)
get_item_n(dgirt_in, by = "year")
get_n(dgirt_in, by = c("year", "source"))

# fit model
dgirt_out <- dgirt(dgirt_in, iter = 2000, chains = 2, cores = 2, seed = 42)

# examine results
summary(dgirt_out, pars = "theta_bar", probs = NULL)[[1]][, c("n_eff", "Rhat")]
get_posterior_mean(dgirt_out, pars = "theta_bar")
apply(as.array(dgirt_out, pars = "theta_bar"), 3, mean)
```
