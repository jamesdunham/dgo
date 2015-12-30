<!-- README.md is generated from README.Rmd. Please edit that file -->
dgirt
=====

dgirt is an R package for dynamic group-level IRT models, as developed in [Caughey and Warshaw 2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html):

> Over the past eight decades, millions of people have been surveyed on their political opinions. Until recently, however, polls rarely included enough questions in a given domain to apply scaling techniques such as IRT models at the individual level, preventing scholars from taking full advantage of historical survey data. To address this problem, we develop a Bayesian group-level IRT approach that models latent traits at the level of demographic and/or geographic groups rather than individuals. We use a hierarchical model to borrow strength cross-sectionally and dynamic linear models to do so across time. The group-level estimates can be weighted to generate estimates for geographic units. This framework opens up vast new areas of research on historical public opinion, especially at the subnational level.

Installation
------------

``` r
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("jamesdunham/dgirt@milestone")
#> Downloading GitHub repo jamesdunham/dgirt@milestone
#> Installing dgirt
#> '/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
#>   --no-environ --no-save --no-restore CMD INSTALL  \
#>   '/private/var/folders/2p/_d3c95qd6ljg28j1f5l2jqxm0000gn/T/RtmpsRsqv8/devtools45072cf00a11/jamesdunham-dgirt-a36dcf5'  \
#>   --library='/Library/Frameworks/R.framework/Versions/3.2/Resources/library'  \
#>   --install-tests 
#> 
#> Reloading installed dgirt
```

Get updates by reinstalling. dgirt is in early stages and under development. See [NEWS](NEWS.md).

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

-   `items`: Names of item response variables \(y_{iq}\) in `data$level1`.
-   `groups`: Names of respondent characteristic variables in `data$level1`. (Note: at this time, `wrangle` requires that the user exclude the geographic indicator from `groups` and name it instead in `geo_id`. Modeling any group predictor is coming.)
-   `time_id`: Name of time period variable in `data$level1`.
-   `geo_id`: Name of geographic identifier variable in `data$level1`.
-   `survey_id`: Name of survey identifier variable in `data$level1`.
-   `survey_weight`: Name of weight variable \(w_i\) in `data$level1`.

The names of the item response variables start with "q\_", so we'll pass them using `grep`.

``` r
state_opinion_fmt = wrangle(
  data = list(level1 = state_opinion),
  vars = list(items = grep("^Q_", colnames(state_opinion), value = TRUE),
              groups = c("female", "race"),
              time_id = "year",
              geo_id = "state",
              survey_id = "source",
              survey_weight = "weight"))
#> 0 rows have no responses
```

`wrangle` returns a list of objects that `dgirt` expects as its first argument. We'll also set its `n_iter` and `n_chain` arguments to minimize its run time, but otherwise rely on the defaults.

``` r
dgirt_estimates = dgirt(state_opinion_fmt, n_iter = 10, n_chain = 1)
#> Started: Mon Dec 21 15:35:45 2015
#> Running 10 iterations in each of 1 chains. Thinning at an interval of 1 with 7 adaptation iterations.
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
#> #  Elapsed Time: 1.70047 seconds (Warm-up)
#> #                0.311216 seconds (Sampling)
#> #                2.01169 seconds (Total)
#> The following numerical problems occured the indicated number of times after warmup on chain 1
#>                                                                                      count
#> validate transformed params: disc[1] is nan, but must be greater than or equal to 0      4
#> Exception thrown at line 196: normal_log: Scale parameter is 0, but must be > 0!         1
#> validate transformed params: disc[18] is nan, but must be greater than or equal to 0     1
#> When a numerical problem occurs, the Metropolis proposal gets rejected.
#> However, by design Metropolis proposals sometimes get rejected even when there are no numerical problems.
#> Thus, if the number in the 'count' column is small, do not ask about this message on stan-users.
#> Ended: Mon Dec 21 15:35:48 2015
```

`dgirt` calls `rstan`, which reports any problems it encounters when compiling the model and sampling. If sampling is successful, `dgirt` returns a `stanfit` object.

To extract samples from the result of `dgirt` we can use `extract_dgirt`, which returns a list whose elements are named arrays and matrices containing estimates of model parameters. We'll take a look at `theta_bar`, the group means (\(\\bar{\\theta}_g\)).

``` r
dgirt_extract = extract_dgirt(dgirt_estimates)
str(dgirt_extract$theta_bar)
#>  num [1:3, 1:9, 1:306] 0.566 0.566 0.566 0.369 0.369 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iterations: NULL
#>   ..$           : chr [1:9] "2006" "2007" "2008" "2009" ...
#>   ..$           : chr [1:306] "0_1_x_AK" "0_1_x_AL" "0_1_x_AR" "0_1_x_AZ" ...
```

`theta_bar` is 3 \(\\times\) 9 \(\\times\) 306:

-   The first dimension of the array is indexed by sampler iteration. We ran the sampler for 10 iterations and by default discarded 7 as warm-ups, so this dimension is length-3.
-   The array's second dimension is indexed by time period, here the nine years 2006-2014.
-   The third dimension, indexed by covariate combination, is length-306, or 2 \(\\times\) 3 \(\\times\) 51. We have 2 levels in our first covariate `D_gender`, 3 levels (excluding `NA`) in our second covariate `D_race_new`, and 51 levels in the geographic identifier.

`cmdstan`
---------

We can use the `method` argument of `dgirt` to choose an alternative to MCMC sampling if `cmdstan` is available. See <http://mc-stan.org/interfaces/cmdstan.html> for installation instructions. For example, setting `method = "optimize"` will call `cmdstan optimize`.

``` r
point_estimates = dgirt(state_opinion_fmt, n_iter = 100, method = "optimize",
  init_range = 1)
#> Started: Mon Dec 21 15:35:48 2015
#> Reading results from disk.
#> Ended: Mon Dec 21 15:37:00 2015
head(point_estimates$theta_bar)
#> Source: local data frame [6 x 7]
#> 
#>           param    value index     t   geo group_1 group_2
#>          (fctr)    (dbl) (chr) (chr) (chr)   (chr)   (chr)
#> 1 theta_bar.1.1 1.989150   1.1  2006    AK       0       1
#> 2 theta_bar.2.1 1.935640   2.1  2007    AK       0       1
#> 3 theta_bar.3.1 0.808958   3.1  2008    AK       0       1
#> 4 theta_bar.4.1 1.735320   4.1  2009    AK       0       1
#> 5 theta_bar.5.1 4.015730   5.1  2010    AK       0       1
#> 6 theta_bar.6.1 3.634120   6.1  2011    AK       0       1
```

`poststratify`
--------------

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
```

``` r
# theta_bars = point_estimates$theta_bar %>%
#   dplyr::rename(female = group_1, race = group_2, year = t) %>%
#   dplyr::mutate(year = as.integer(year), state = as.factor(geo), female = as.factor(female), race = as.factor(race))
# group_means = poststratify(
#   group_means = theta_bars,
#   targets =  state_demographics, 
#   variables = "year",
#   prop_var = "proportion")
```
