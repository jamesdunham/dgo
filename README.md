<!-- README.md is generated from README.Rmd. Please edit that file -->
dgirt
=====

dgirt is an R package for dynamic group-level IRT models, as developed in [Caughey and Warshaw 2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html):

> Over the past eight decades, millions of people have been surveyed on their political opinions. Until recently, however, polls rarely included enough questions in a given domain to apply scaling techniques such as IRT models at the individual level, preventing scholars from taking full advantage of historical survey data. To address this problem, we develop a Bayesian group-level IRT approach that models latent traits at the level of demographic and/or geographic groups rather than individuals. We use a hierarchical model to borrow strength cross-sectionally and dynamic linear models to do so across time. The group-level estimates can be weighted to generate estimates for geographic units. This framework opens up vast new areas of research on historical public opinion, especially at the subnational level.

Installation
------------

    devtools::install_github("jamesdunham/dgirt")

Get updates by reinstalling. dgirt is in early stages and under development. See [NEWS](NEWS.md), last updated 2015-12-30.

Quick start
-----------

-   `wrangle` prepares data
-   `dgirt` fits models
-   `poststratify` reweights estimates

Use
---

`state_opinion` is a dataset included with `dgirt` in which rows correspond to survey responses from individuals.

``` r
library(dgirt)
data(state_opinion)
```

Data formatted in this way need to be restructured with the `wrangle` function. We'll pass `state_opinion` to `wrangle`'s `data` argument, which takes a list of data to be used in modeling. Our table of survey responses will be an element named `level1`, for the lowest hierarchical level in the model. (Note: at the moment, there are limitations on model specifications. Level-one data is required and a second hierarchical level is optional.)

We'll use the `vars` argument to identify variables of importance in `data` (e.g. which represent item responses). `vars` is a list of named character vectors with (at least) these elements:

-   `items`: Names of item response variables in `data$level1`.
-   `groups`: Names of respondent characteristic variables in `data$level1`. (Note: at this time, `wrangle` requires that the user exclude the geographic indicator from `groups` and name it instead in `geo_id`. Modeling any group predictor is coming.)
-   `time_id`: Name of time period variable in `data$level1`.
-   `geo_id`: Name of geographic identifier variable in `data$level1`.
-   `survey_id`: Name of survey identifier variable in `data$level1`.
-   `survey_weight`: Name of weight variable in `data$level1`.

The names of the item response variables start with "Q\_", so we'll pass them using `grep`.

``` r
state_opinion_fmt = wrangle(
  data = list(level1 = state_opinion),
  vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
              groups = c("race"),
              time_id = "year",
              geo_id = "state",
              survey_id = "source",
              survey_weight = "weight"),
  filters = list(periods = c(2006:2010)))
```

This output omits verbose messages. `wrangle` returns a list of objects that `dgirt` expects as its first argument. We'll also set its `n_iter` and `n_chain` arguments to minimize its run time, but otherwise rely on the defaults.

`dgirt()` calls `rstan()`, which reports any problems it encounters when compiling the model and sampling. Reporting is verbose and not all messages indicate problems. If sampling is successful, `dgirt()` returns an object of class `stanfit`. (See rstan documentation.)

A short trial run is often a good idea.

``` r
dgirt_estimates = dgirt(state_opinion_fmt, n_iter = 10, n_chain = 1)
#> 
#> SAMPLING FOR MODEL '605ba6820a8c93e8038f6394fbb2c3e1' NOW (CHAIN 1).
#> 
#> Chain 1, Iteration: 1 / 10 [ 10%]  (Warmup)
#> Chain 1, Iteration: 2 / 10 [ 20%]  (Warmup)
#> Chain 1, Iteration: 3 / 10 [ 30%]  (Warmup)
#> Chain 1, Iteration: 4 / 10 [ 40%]  (Warmup)
#> Chain 1, Iteration: 5 / 10 [ 50%]  (Warmup)
#> Chain 1, Iteration: 6 / 10 [ 60%]  (Warmup)
#> Chain 1, Iteration: 7 / 10 [ 70%]  (Warmup)
#> Chain 1, Iteration: 8 / 10 [ 80%]  (Sampling)
#> Chain 1, Iteration: 9 / 10 [ 90%]  (Sampling)
#> Chain 1, Iteration: 10 / 10 [100%]  (Sampling)
#> #  Elapsed Time: 2.12652 seconds (Warm-up)
#> #                0.046305 seconds (Sampling)
#> #                2.17282 seconds (Total)
```

We omit verbose messages here. Now, a longer run:

    # Not run
    dgirt_estimates = dgirt(state_opinion_fmt, n_iter = 2000, n_chain = 4)

To examine the the `dgirt()` results we can use `extract_dgirt()`, which attaches labels to the saved parameters according to the variable names originally passed to `wrangle()` and any factor levels. Right now, `extract_dgirt()` shows only the posterior means.

Note that `dgirt()` returns a `stanfit` object when `method = "rstan"` (as above, the default) and a list of point estimates if `method = "optimize"` (details below); `extract_dgirt()` only works with `stanfit` objects. (The inconsistency in return types isn't desirable and will change in the future.)

The group means can be found as `theta_bar`.

``` r
dgirt_extract = extract_dgirt(dgirt_estimates, state_opinion_fmt)
#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1
head(dgirt_extract$theta_bar)
#> Source: local data frame [6 x 6]
#> 
#>    Var1  Var2      value  year  state   race
#>   (int) (int)      (dbl) (dbl) (fctr) (fctr)
#> 1     1     1 -6.2806986  2006     AK  white
#> 2     2     1 -8.3272319  2007     AK  white
#> 3     3     1 -5.8700206  2008     AK  white
#> 4     4     1 -5.8248482  2009     AK  white
#> 5     5     1 -4.4962816  2010     AK  white
#> 6     1     2 -0.8074506  2006     AL  white
```

`cmdstan`
---------

We can use the `method` argument of `dgirt` to choose an alternative to MCMC sampling if `cmdstan` is available. See <http://mc-stan.org/interfaces/cmdstan.html> for installation instructions. For example, setting `method = "optimize"` will call `cmdstan optimize`.

First, a trial run.

``` r
optimize_estimates = dgirt(state_opinion_fmt, n_iter = 20, method = "optimize",
  init_range = 0.5)
#> Started: Thu Jan 21 09:49:01 2016
#> Reading results from disk.
#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1

#> Warning: Truncating vector to length 1
#> Ended: Thu Jan 21 09:49:03 2016
head(optimize_estimates$theta_bar)
#> Source: local data frame [6 x 5]
#> 
#>           param     value  year  state   race
#>          (fctr)     (dbl) (dbl) (fctr) (fctr)
#> 1 theta_bar.1.1 -0.517930  2006     AK  white
#> 2 theta_bar.2.1 -1.147970  2007     AK  white
#> 3 theta_bar.3.1 -0.530355  2008     AK  white
#> 4 theta_bar.4.1 -0.111352  2009     AK  white
#> 5 theta_bar.5.1 -0.562063  2010     AK  white
#> 6 theta_bar.1.2 -0.382143  2006     AL  white
```

And now a longer run.

    # Not run
    optimize_estimates = dgirt(state_opinion_fmt, n_iter = 20000,
      method = "optimize", init_range = 0.5)

`poststratify`
--------------

`poststratify()` can reweight estimates from `dgirt()` (if `method = "optimize"`) or `extract_dgirt()` (if `method = "rstan"`, the default). `postratify()` returns weighted means for groups or arbitrary aggregations of groups.

The `state_demographics` dataset contains population proportions for demographic strata by year. At the moment, it's necessary to relabel the group factor levels in the `dgirt()` results to match those in the population proportion data. And the time variable in the `dgirt()` results needs to be recast as integer.

``` r
data(state_demographics)
head(state_demographics)
#> Source: local data frame [6 x 7]
#> 
#>    state  year              race female education   age   proportion
#>   (fctr) (int)            (fctr) (fctr)     (int) (int)        (dbl)
#> 1     AK  1960 White or Hispanic   Male         1     1 8.857296e-05
#> 2     AL  1960 White or Hispanic   Male         1     1 6.986948e-04
#> 3     AR  1960 White or Hispanic   Male         1     1 3.831912e-04
#> 4     AZ  1960 White or Hispanic   Male         1     1 3.518153e-04
#> 5     CA  1960 White or Hispanic   Male         1     1 3.463380e-03
#> 6     CO  1960 White or Hispanic   Male         1     1 3.543790e-04
optimize_estimates$theta_bar$race = factor(optimize_estimates$theta_bar$race, labels = c("White or Hispanic", "Black", "Other"))
optimize_estimates$theta_bar$year = as.integer(optimize_estimates$theta_bar$year)
```

Now we pass these data, the same `groups` argument as used originally with `wrangle`, and a vector of variable names as `strata` that define aggregations of interest in the data. For exposition we'll set two optional variables. We give the name of the variable in the demographic data for the population proportion as `prop_var`. And passing a variable name to `check_proportions` will test the demographic data for whether population proportions sum to one within groups defined by the values of that variable.

``` r
dgirt_extract$theta_bar$race = factor(dgirt_extract$theta_bar$race, labels = c("White or Hispanic", "Black", "Other"))
dgirt_extract$theta_bar$year = as.integer(dgirt_extract$theta_bar$year)
group_means = poststratify(
  group_means = dgirt_extract$theta_bar,
  targets =  state_demographics,
  groups = c("race"),
  strata = c("state", "year"),
  prop_var = "proportion",
  check_proportions = "year")
#> Warning in poststratify(group_means = dgirt_extract$theta_bar, targets =
#> state_demographics, : More rows of proportions than combinations of its
#> strata and grouping variables. Summing proportions over other variables.
head(group_means)
#> Source: local data frame [6 x 3]
#> 
#>    state  year       value
#>   (fctr) (int)       (dbl)
#> 1     AK  2006 -0.01504757
#> 2     AK  2007 -0.02031446
#> 3     AK  2008 -0.01584997
#> 4     AK  2009 -0.01935265
#> 5     AK  2010 -0.01149973
#> 6     AL  2006 -0.03762232
```

The same approach works after `dgirt()` if `method = "optimize")`.

``` r
optimize_estimates$theta_bar$race = factor(optimize_estimates$theta_bar$race, labels = c("White or Hispanic", "Black", "Other"))
optimize_estimates$theta_bar$year = as.integer(optimize_estimates$theta_bar$year)
optimize_group_means = poststratify(
  group_means = optimize_estimates$theta_bar,
  targets =  state_demographics,
  groups = c("race"),
  strata = c("state", "year"),
  prop_var = "proportion",
  check_proportions = "year")
#> Warning in poststratify(group_means = optimize_estimates$theta_bar, targets
#> = state_demographics, : More rows of proportions than combinations of its
#> strata and grouping variables. Summing proportions over other variables.
head(optimize_group_means)
#> Source: local data frame [6 x 3]
#> 
#>    state  year        value
#>   (fctr) (int)        (dbl)
#> 1     AK  2006 -0.002496122
#> 2     AK  2007 -0.003602109
#> 3     AK  2008 -0.003149042
#> 4     AK  2009 -0.002535193
#> 5     AK  2010 -0.002642950
#> 6     AL  2006 -0.015120944
```

`plot_means`
------------

We can quickly plot group means with `plot_means`. It can handle the `theta_bar` element of the value of `poststratify()`. (Figures omitted.)

``` r
plot_means(group_means, "year", "state", jitter = TRUE)
```

Or that of `dgirt_extract()` where dgirt(`method = "rstan"`),

``` r
plot_means(dgirt_extract$theta_bar, "year", "state", jitter = TRUE)
```

Or the value of `dgirt()` in the case of `method = "optimize"`.

``` r
plot_means(optimize_estimates$theta_bar, "year", "state", jitter = TRUE)
```
