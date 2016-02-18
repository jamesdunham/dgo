<!-- README.md is generated from README.Rmd. Please edit that file -->
dgirt
=====

[![Build Status](https://travis-ci.org/jamesdunham/dgirt.svg?branch=master)](https://travis-ci.org/jamesdunham/dgirt)

dgirt is an R package for dynamic group-level IRT models, as developed in [Caughey and Warshaw 2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html):

> Over the past eight decades, millions of people have been surveyed on their political opinions. Until recently, however, polls rarely included enough questions in a given domain to apply scaling techniques such as IRT models at the individual level, preventing scholars from taking full advantage of historical survey data. To address this problem, we develop a Bayesian group-level IRT approach that models latent traits at the level of demographic and/or geographic groups rather than individuals. We use a hierarchical model to borrow strength cross-sectionally and dynamic linear models to do so across time. The group-level estimates can be weighted to generate estimates for geographic units. This framework opens up vast new areas of research on historical public opinion, especially at the subnational level.

Installation
------------

``` r
# devtools::install_github("jamesdunham/dgirt")
```

Get updates by reinstalling. dgirt is in early stages and under development. See [NEWS](NEWS.md), last updated 2016-01-28.

Quick start
-----------

-   `wrangle` prepares data
-   `dgirt` fits models
-   `poststratify` reweights estimates

Use
---

`state_opinion` is a dataset included with `dgirt` in which rows correspond to survey responses from individuals.

``` r
# library(dgirt)
devtools::load_all()
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
wrangle_output = wrangle(
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

``` r
dgirt_output = dgirt(wrangle_output, n_iter = 10, n_chain = 1)
#> 
#> SAMPLING FOR MODEL '84f408a951e786210b324c1e54eefa8c' NOW (CHAIN 1).
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
#> Chain 1, Iteration: 10 / 10 [100%]  (Sampling)# 
#> #  Elapsed Time: 0.394312 seconds (Warm-up)
#> #                1.18672 seconds (Sampling)
#> #                1.58103 seconds (Total)
#> #
```

Before examining the `dgirt()` output we might want to name its elements.

``` r
named_dgirt_output = name(dgirt_output, wrangle_output)
```

The group means can be found as an array `theta_bar`, which is samples x periods x groups. We can `apply` over the sampler iterations (just the 3 in 10 kept from our short run, in this example) to summarize the posterior.

``` r
str(named_dgirt_output$theta_bar)
#>  num [1:3, 1:5, 1:153] 7.39 7.22 7.27 14.45 12.93 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iterations: NULL
#>   ..$ time      : chr [1:5] "2006" "2007" "2008" "2009" ...
#>   ..$ group     : chr [1:153] "black_x_AK" "black_x_AL" "black_x_AR" "black_x_AZ" ...
group_posterior_means = apply(named_dgirt_output$theta_bar, 2:3, mean)
group_posterior_means[2:3, 1:5]
#>       group
#> time   black_x_AK black_x_AL black_x_AR black_x_AZ black_x_CA
#>   2007   13.50196   13.47203   13.49858   13.50428   13.47893
#>   2008   13.35576   13.94017   12.29895   15.84052   12.98455
```

Alternatively, `apply_dgirt()` can apply a function like `mean` over the posteriors of all `dgirt` parameters at once.

``` r
posterior_means = apply_dgirt(dgirt_output, wrangle_output, fun = 'mean')
posterior_means$theta_bar
#> Source: local data frame [765 x 6]
#> 
#>     Var1  Var2      value  year  state   race
#>    (int) (int)      (dbl) (dbl) (fctr) (fctr)
#> 1      1     1   7.296147  2006     AK  black
#> 2      2     1  13.501962  2007     AK  black
#> 3      3     1  13.355764  2008     AK  black
#> 4      4     1  62.337810  2009     AK  black
#> 5      5     1 309.667345  2010     AK  black
#> 6      1     2   7.297617  2006     AL  black
#> 7      2     2  13.472031  2007     AL  black
#> 8      3     2  13.940167  2008     AL  black
#> 9      4     2  65.152305  2009     AL  black
#> 10     5     2 323.528165  2010     AL  black
#> ..   ...   ...        ...   ...    ...    ...
```

`cmdstan`
---------

We can use the `method` argument of `dgirt` to choose an alternative to MCMC sampling if `cmdstan` is available. See <http://mc-stan.org/interfaces/cmdstan.html> for installation instructions. For example, setting `method = "optimize"` will call `cmdstan optimize`.

``` r
optimize_output = dgirt(wrangle_output, n_iter = 20, method = "optimize", init_range = 0.5)
#> Started: Thu Feb 18 15:52:19 2016
#> Reading results from disk.
#> Ended: Thu Feb 18 15:52:21 2016
head(optimize_output$theta_bar)
#> Source: local data frame [6 x 5]
#> 
#>           param     value  year  state   race
#>          (fctr)     (dbl) (dbl) (fctr) (fctr)
#> 1 theta_bar.1.1 1.1278800  2006     AK  black
#> 2 theta_bar.2.1 1.1025000  2007     AK  black
#> 3 theta_bar.3.1 0.6897520  2008     AK  black
#> 4 theta_bar.4.1 0.0971638  2009     AK  black
#> 5 theta_bar.5.1 0.8052460  2010     AK  black
#> 6 theta_bar.1.2 0.6783520  2006     AL  black
```

`poststratify`
--------------

`poststratify()` can reweight estimates from `dgirt()` (if `method = "optimize"`) or `extract_dgirt()` (if `method = "rstan"`, the default). `postratify()` returns weighted means for groups or arbitrary aggregations of groups.

The `state_demographics` dataset contains population proportions for demographic strata by year. At the moment, it's necessary to relabel the group factor levels in the `dgirt()` results to match those in the population proportion data.

``` r
data(state_demographics)
head(state_demographics)
#> Source: local data frame [6 x 7]
#> 
#>    state  year              race female education   age   proportion
#>   (fctr) (int)            (fctr) (fctr)     (int) (int)        (dbl)
#> 1     AK  1960 white or hispanic   male         1     1 8.857296e-05
#> 2     AL  1960 white or hispanic   male         1     1 6.986948e-04
#> 3     AR  1960 white or hispanic   male         1     1 3.831912e-04
#> 4     AZ  1960 white or hispanic   male         1     1 3.518153e-04
#> 5     CA  1960 white or hispanic   male         1     1 3.463380e-03
#> 6     CO  1960 white or hispanic   male         1     1 3.543790e-04
state_demographics$race = factor(state_demographics$race, labels = c("white", "black", "other"))
```

Now we pass these data, the same `groups` argument as used originally with `wrangle`, and a vector of variable names as `strata` that define aggregations of interest in the data. For exposition we'll set two optional variables. We give the name of the variable in the demographic data for the population proportion as `prop_var`. And passing a variable name to `summands` will test the demographic data for whether population proportions sum to one within groups defined by the values of that variable.

``` r
posterior_means$theta_bar$year = as.integer(posterior_means$theta_bar$year)
poststratify_output = poststratify(
  group_means = posterior_means$theta_bar,
  targets =  state_demographics,
  groups = c("race"),
  strata = c("state", "year"),
  prop_var = "proportion",
  summands = "year")
#> Warning in poststratify(group_means = posterior_means$theta_bar, targets
#> = state_demographics, : More rows of proportions than combinations of its
#> strata and grouping variables. Summing proportions over other variables.
#> Warning in inner_join_impl(x, y, by$x, by$y): joining factors with
#> different levels, coercing to character vector
head(poststratify_output)
#> Source: local data frame [6 x 3]
#> 
#>    state  year      value
#>   (fctr) (int)      (dbl)
#> 1     AK  2006   7.298276
#> 2     AK  2007  13.485674
#> 3     AK  2008  13.851239
#> 4     AK  2009  64.845489
#> 5     AK  2010 320.245700
#> 6     AL  2006   7.298520
```

The same approach works after `dgirt()` if `method = "optimize"`.

``` r
optimize_output$theta_bar$year = as.integer(optimize_output$theta_bar$year)
optimize_poststratify_output = poststratify(
  group_means = optimize_output$theta_bar,
  targets =  state_demographics,
  groups = c("race"),
  strata = c("state", "year"),
  prop_var = "proportion",
  summands = "year")
#> Warning in poststratify(group_means = optimize_output$theta_bar, targets
#> = state_demographics, : More rows of proportions than combinations of its
#> strata and grouping variables. Summing proportions over other variables.
#> Warning in inner_join_impl(x, y, by$x, by$y): joining factors with
#> different levels, coercing to character vector
head(optimize_poststratify_output)
#> Source: local data frame [6 x 3]
#> 
#>    state  year       value
#>   (fctr) (int)       (dbl)
#> 1     AK  2006 0.591092039
#> 2     AK  2007 0.329401870
#> 3     AK  2008 0.258412869
#> 4     AK  2009 0.002827527
#> 5     AK  2010 0.394233455
#> 6     AL  2006 0.113420651
```

`plot_means`
------------

We can quickly plot group means with `plot_means`. It can handle the `theta_bar` element of the value of `poststratify()`. (Figures omitted.)

``` r
plot_means(poststratify_output, "year", "state", jitter = TRUE)
```

Or that of `apply_dgirt()` where dgirt(`method = "rstan"`),

``` r
plot_means(posterior_means$theta_bar, "year", "state", jitter = TRUE)
```

Or the value of `dgirt()` in the case of `method = "optimize"`.

``` r
plot_means(optimize_output$theta_bar, "year", "state", jitter = TRUE)
```
