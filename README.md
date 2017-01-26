[![Build Status](https://travis-ci.org/jamesdunham/dgo.svg?branch=master)](https://travis-ci.org/jamesdunham/dgo) [![Build status](https://ci.appveyor.com/api/projects/status/1ta36kmoqen98k87?svg=true)](https://ci.appveyor.com/project/jamesdunham/dgo)

dgo is an R package for the dynamic estimation of group-level opinion. The package can be used to estimate subpopulation groups' average latent conservatism (or other latent trait) from individuals' responses to dichotomous questions using a Bayesian group-level IRT approach developed by [Caughey and Warshaw 2015](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html) that models latent traits at the level of demographic and/or geographic groups rather than individuals. This approach uses a hierarchical model to borrow strength cross-sectionally and dynamic linear models to do so across time. The group-level estimates can be weighted to generate estimates for geographic units, such as states.

dgo can also be used to estimate smoothed estimates of subpopulation groups' average responses on individual survey questions using a dynamic multi-level regression and poststratification (MRP) model ([Park, Gelman, and Bafumi 2004](http://stat.columbia.edu/~gelman/research/published/StateOpinionsNationalPolls.050712.dkp.pdf)). For instance, it could be used to estimate public opinion in each state on same-sex marriage or the Affordable Care Act.

This model opens up new areas of research on historical public opinion in the United States at the subnational level. It also enables scholars of comparative politics to estimate dynamic models of public opinion opinion at the country or subnational level.

Prerequisites
=============

Installation requires [RStan](http://mc-stan.org/interfaces/rstan.html) and its prerequisites, in particular a C++ toolchain. If you don't have RStan, follow its "[Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)" guide before continuing.

Installation
============

dgo can be installed from [GitHub](https://github.com/jamesdunham/dgo) using [devtools](https://github.com/hadley/devtools/):

``` r
devtools::install_github("jamesdunham/dgo", dependencies = TRUE)
```

Getting started
===============

``` r
library(dgo)
#> Loading required package: dgodata
#> Loading required package: Rcpp
#> Loading required package: rstan
#> Loading required package: ggplot2
#> Loading required package: StanHeaders
#> rstan (Version 2.14.1, packaged: 2016-12-28 14:55:41 UTC, GitRev: 5fa1e80eb817)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> rstan_options(auto_write = TRUE)
#> options(mc.cores = parallel::detectCores())
#> 
#> Attaching package: 'dgo'
#> The following objects are masked from 'package:dgodata':
#> 
#>     toy_dgirtfit, toy_dgirt_in
```

The minimal workflow from raw data to estimation is:

1.  shape input data using the `shape` function; and
2.  pass the result to the `dgirt` function to estimate a latent trait (e.g., conservatism) or `dgmrp` function to estimate opinion on a single survey question.

### Set RStan options

These are RStan's recommended options on a local, multicore machine with excess RAM:

``` r
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```

Abortion Attitudes
------------------

### Prepare input data with `shape`

DGIRT models are *dynamic*, so we need to specify which variable in the data represents time. They are also *group-level*, with groups defined by one variable for respondents' local geographic area and one or more variables for respondent characteristics.

The `time_filter` and `geo_filter` arguments optionally subset the data. Finally, `shape` requires the names of the survey identifier and survey weight variables in the data.

``` r
dgirt_in_abortion <- shape(opinion,
                  item_names = "abortion",
                  time_name = "year",
                  geo_name = "state",
                  group_names = "race3",
                  geo_filter = c("CA", "GA", "LA", "MA"),
                  survey_name = "source",
                  weight_name = "weight")
#> Applying restrictions, pass 1...
#>  Dropped 5 rows for missingness in covariates
#>  Dropped 633 rows for lacking item responses
#> Applying restrictions, pass 2...
#>  No changes
```

The reshaped and subsetted data can be summarized in a few ways before model fitting.

``` r
summary(dgirt_in_abortion)
#> Items:
#> [1] "abortion"
#> Respondents:
#>    23,007 in `item_data`
#> Grouping variables:
#> [1] "year"  "state" "race3"
#> Time periods:
#> [1] 2006 2007 2008 2009 2010
#> Local geographic areas:
#> [1] "CA" "GA" "LA" "MA"
#> Hierarchical parameters:
#> [1] "GA"         "LA"         "MA"         "race3other" "race3white"
#> Modifiers of hierarchical parameters:
#> character(0)
#> Constants:
#>  Q  T  P  N  G  H  D 
#>  1  5  5 60 12  1  1
```

Response counts by survey-year:

``` r
get_n(dgirt_in_abortion, by = c("year", "source"))
#>    year    source    n
#> 1: 2006 CCES_2006 5275
#> 2: 2007 CCES_2007 1690
#> 3: 2008 CCES_2008 4697
#> 4: 2009 CCES_2009 2141
#> 5: 2010 CCES_2010 9204
```

Response counts by item-year:

``` r
get_item_n(dgirt_in_abortion, by = "year")
#>    year abortion
#> 1: 2006     5275
#> 2: 2007     1690
#> 3: 2008     4697
#> 4: 2009     2141
#> 5: 2010     9204
```

### Fit a model with `dgirt` or `dgmrp`

`dgirt` and `dgmrp` fit estimation models to data from `shape`. `dgirt` can be used to estimate a latent variable based on responses to multiple survey questions (e.g., latent policy conservatism), while `dgmrp` can be used to estimate public opinion on an individual survey question (e.g., abortion) using a dynamic multi-level regression and post-stratification (MRP) model. In this case, we use `dgmrp` to model abortion attitudes.

Under the hood, these functions use RStan for MCMC sampling, and arguments can be passed to RStan's `stan` via the `...` argument of `dgirt` and `dgmrp`. This will almost always be desirable, at a minimum to specify the number of sampler iterations, chains, and cores.

``` r
dgirt_out_abortion <- dgmrp(dgirt_in_abortion, iter = 1500, chains = 4, cores =
  4, seed = 42)
```

The model results are held in a `dgirtfit` object. Methods from RStan like `extract` are available if needed because `dgirtfit` is a subclass of `stanfit`. But dgo provides its own methods for typical post-estimation tasks.

### Work with `dgirt` or `dgmrp` results

For a high-level summary of the result, use `summary`.

``` r
summary(dgirt_out_abortion)
#> dgirt samples from 4 chains of 1500 iterations, 750 warmup, thinned every 1 
#>   Drawn Thu Jan 26 08:59:57 2017 
#>   Package version 0.2.8 
#>   Model version 2017_01_04_singleissue 
#>   117 parameters; 60 theta_bars (year, state and race3)
#>   5 periods 2006 to 2010 
#> 
#> n_eff
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   53.52  364.00  752.80 1011.00 1392.00 3000.00
#> 
#> Rhat
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.9994  1.0020  1.0040  1.0070  1.0090  1.0850
#> 
#> Elapsed time
#>    chain warmup sample total
#> 1:     1     9S    10S   19S
#> 2:     2    11S    15S   26S
#> 3:     3     8S    11S   19S
#> 4:     4     9S    15S   24S
```

To summarize posterior samples, use `summarize`. The default output gives summary statistics for the `theta_bar` parameters, which represent the mean of the latent outcome for the groups defined by time, local geographic area, and the demographic characteristics specified in the earlier call to `shape`.

``` r
head(summarize(dgirt_out_abortion))
#>        param state race3 year      mean         sd    median     q_025
#> 1: theta_bar    CA black 2006 0.7169682 0.07805130 0.7173998 0.5654617
#> 2: theta_bar    CA black 2007 0.7475491 0.10839806 0.7415758 0.5537648
#> 3: theta_bar    CA black 2008 0.4831040 0.07608744 0.4819045 0.3354228
#> 4: theta_bar    CA black 2009 0.4090061 0.07179438 0.4068443 0.2715811
#> 5: theta_bar    CA black 2010 0.5311185 0.07702568 0.5339432 0.3757688
#> 6: theta_bar    CA other 2006 0.5631683 0.07651700 0.5604350 0.4174244
#>        q_975
#> 1: 0.8678976
#> 2: 0.9692746
#> 3: 0.6322093
#> 4: 0.5521007
#> 5: 0.6778760
#> 6: 0.7134687
```

Alternatively, `summarize` can apply arbitrary functions to posterior samples for whatever parameter is given by its `pars` argument. Enclose function names with quotes. For convenience, `"q_025"` and `"q_975"` give the 2.5th and 97.5th posterior quantiles.

``` r
summarize(dgirt_out_abortion, pars = "xi", funs = "var")
#>    param year        var
#> 1:    xi 2006 0.02077508
#> 2:    xi 2007 0.03237822
#> 3:    xi 2008 0.03029146
#> 4:    xi 2009 0.02565337
#> 5:    xi 2010 0.02735791
```

To access posterior samples in tabular form use `as.data.frame`. By default, this method returns post-warmup samples for the `theta_bar` parameters, but like other methods takes a `pars` argument.

``` r
head(as.data.frame(dgirt_out_abortion))
#>        param state race3 year iteration     value
#> 1: theta_bar    CA black 2006         1 0.7891505
#> 2: theta_bar    CA black 2006         2 0.7297598
#> 3: theta_bar    CA black 2006         3 0.7416201
#> 4: theta_bar    CA black 2006         4 0.5768916
#> 5: theta_bar    CA black 2006         5 0.8729594
#> 6: theta_bar    CA black 2006         6 0.8582627
```

To poststratify the results use `poststratify`. The following example uses the group population proportions bundled as `annual_state_race_targets` to reweight and aggregate estimates to strata defined by state-years.

Read `help("poststratify")` for more details.

``` r
poststratify(dgirt_out_abortion, annual_state_race_targets, strata_names =
  c("state", "year"), aggregated_names = "race3")
#>     state year       value
#>  1:    CA 2006  0.55743394
#>  2:    CA 2007  0.58833579
#>  3:    CA 2008  0.36830483
#>  4:    CA 2009  0.33078589
#>  5:    CA 2010  0.47437626
#>  6:    GA 2006  0.32171046
#>  7:    GA 2007  0.19477981
#>  8:    GA 2008  0.03076044
#>  9:    GA 2009  0.01994895
#> 10:    GA 2010  0.09501160
#> 11:    LA 2006 -0.04148402
#> 12:    LA 2007 -0.21515928
#> 13:    LA 2008 -0.22757974
#> 14:    LA 2009 -0.18104924
#> 15:    LA 2010 -0.14084000
#> 16:    MA 2006  0.70717604
#> 17:    MA 2007  0.91978031
#> 18:    MA 2008  0.52434478
#> 19:    MA 2009  0.42477870
#> 20:    MA 2010  0.53311650
```

To plot the results use `dgirt_plot`. This method plots summaries of posterior samples by time period. By default, it shows a 95% credible interval around posterior medians for the `theta_bar` parameters, for each local geographic area. For this (unconverged) toy example we omit the CIs.

``` r
dgirt_plot(dgirt_out_abortion, y_min = NULL, y_max = NULL)
```

![](README/unnamed-chunk-14-1.png)

Output from `dgirt_plot` can be customized to some extent using objects from the ggplot2 package.

``` r
dgirt_plot(dgirt_out_abortion, y_min = NULL, y_max = NULL) + theme_classic()
```

![](README/unnamed-chunk-15-1.png)

`dgirt_plot` can also plot the `data.frame` output from `poststratify`. This requires arguments that identify the relevant variables in the `data.frame`. Below, `poststratify` aggregates over the demographic grouping variable `race3`, resulting in a `data.frame` of estimates by state-year. So, in the subsequent call to `dgirt_plot`, we pass the names of the state and year variables. The `group_names` argument is `NULL` because there are no grouping variables left after aggregating over `race3`.

``` r
ps <- poststratify(dgirt_out_abortion, annual_state_race_targets, strata_names =
  c("state", "year"), aggregated_names = "race3")
head(ps)
#>    state year     value
#> 1:    CA 2006 0.5574339
#> 2:    CA 2007 0.5883358
#> 3:    CA 2008 0.3683048
#> 4:    CA 2009 0.3307859
#> 5:    CA 2010 0.4743763
#> 6:    GA 2006 0.3217105
dgirt_plot(ps, group_names = NULL, time_name = "year", geo_name = "state")
```

![](README/unnamed-chunk-16-1.png)

Policy Liberalism
-----------------

### Prepare input data with `shape`

``` r
dgirt_in_liberalism <- shape(opinion, item_names = c("abortion",
    "affirmative_action","stemcell_research" , "gaymarriage_amendment",
    "partialbirth_abortion") , time_name = "year", geo_name = "state",
  group_names = "race3", geo_filter = c("CA", "GA", "LA", "MA"), survey_name =
    "source", weight_name = "weight")
#> Applying restrictions, pass 1...
#>  Dropped 5 rows for missingness in covariates
#>  Dropped 8 rows for lacking item responses
#> Applying restrictions, pass 2...
#>  No changes
```

The reshaped and subsetted data can be summarized in a few ways before model fitting.

``` r
summary(dgirt_in_liberalism)
#> Items:
#> [1] "abortion"              "affirmative_action"    "gaymarriage_amendment"
#> [4] "partialbirth_abortion" "stemcell_research"    
#> Respondents:
#>    23,632 in `item_data`
#> Grouping variables:
#> [1] "year"  "state" "race3"
#> Time periods:
#> [1] 2006 2007 2008 2009 2010
#> Local geographic areas:
#> [1] "CA" "GA" "LA" "MA"
#> Hierarchical parameters:
#> [1] "GA"         "LA"         "MA"         "race3other" "race3white"
#> Modifiers of hierarchical parameters:
#> character(0)
#> Constants:
#>   Q   T   P   N   G   H   D 
#>   5   5   5 300  12   1   1
```

Response counts by item-year:

``` r
get_item_n(dgirt_in_liberalism, by = "year")
#>    year abortion affirmative_action stemcell_research
#> 1: 2006     5275               4750              2483
#> 2: 2007     1690               1557              1705
#> 3: 2008     4697               4704              4002
#> 4: 2009     2141               2147                 0
#> 5: 2010     9204               9241              9146
#>    gaymarriage_amendment partialbirth_abortion
#> 1:                  2642                  5064
#> 2:                  1163                  1684
#> 3:                  4265                     0
#> 4:                     0                     0
#> 5:                  9226                     0
```

### Fit a model with `dgirt`

`dgirt` and `dgmrp` fit estimation models to data from `shape`. `dgirt` can be used to estimate a latent variable based on responses to multiple survey questions (e.g., latent policy conservatism), while `dgmrp` can be used to estimate public opinion on an individual survey question using a dynamic multi-level regression and post-stratification (MRP) model.

Under the hood, these functions use RStan for MCMC sampling, and arguments can be passed to RStan's `stan` via the `...` argument of `dgirt` and `dgmrp`. This will almost always be desirable, at a minimum to specify the number of sampler iterations, chains, and cores.

``` r
dgirt_out_liberalism <- dgirt(dgirt_in_liberalism, iter = 3000, chains = 4,
  cores = 4, seed = 42)
```

The model results are held in a `dgirtfit` object. Methods from RStan like `extract` are available if needed because `dgirtfit` is a subclass of `stanfit`. But dgo provides its own methods for typical post-estimation tasks.

### Work with `dgirt` results

For a high-level summary of the result, use `summary`.

``` r
summary(dgirt_out_liberalism)
#> dgirt samples from 4 chains of 3000 iterations, 1500 warmup, thinned every 1 
#>   Drawn Thu Jan 26 09:04:36 2017 
#>   Package version 0.2.8 
#>   Model version 2017_01_04 
#>   137 parameters; 60 theta_bars (year, state and race3)
#>   5 periods 2006 to 2010 
#> 
#> n_eff
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   24.25  126.40  288.40  724.70  717.60 6000.00
#> 
#> Rhat
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.000   1.007   1.016   1.024   1.034   1.146
#> 
#> Elapsed time
#>    chain warmup sample   total
#> 1:     1 1M 45S 1M 16S  2M 61S
#> 2:     2 1M 37S 1M 37S  2M 74S
#> 3:     3 1M 53S 2M 34S  3M 87S
#> 4:     4 1M 43S 1M 58S 2M 101S
```

To summarize posterior samples, use `summarize`. The default output gives summary statistics for the `theta_bar` parameters, which represent the mean of the latent outcome for the groups defined by time, local geographic area, and the demographic characteristics specified in the earlier call to `shape`.

``` r
head(summarize(dgirt_out_liberalism))
#>        param state race3 year      mean         sd    median        q_025
#> 1: theta_bar    CA black 2006 0.5424676 0.07682737 0.5292402  0.408601887
#> 2: theta_bar    CA black 2007 0.5762770 0.09779929 0.5565021  0.419725395
#> 3: theta_bar    CA black 2008 0.5508289 0.10531420 0.5275569  0.392452411
#> 4: theta_bar    CA black 2009 0.4836891 0.09271300 0.4762236  0.341245135
#> 5: theta_bar    CA black 2010 0.4442502 0.07279811 0.4404217  0.317057094
#> 6: theta_bar    CA other 2006 0.1457190 0.07393103 0.1547438 -0.008340705
#>        q_975
#> 1: 0.7148904
#> 2: 0.8080642
#> 3: 0.8044522
#> 4: 0.7057762
#> 5: 0.6042427
#> 6: 0.2720437
```

Alternatively, `summarize` can apply arbitrary functions to posterior samples for whatever parameter is given by its `pars` argument. Enclose function names with quotes. For convenience, `"q_025"` and `"q_975"` give the 2.5th and 97.5th posterior quantiles.

``` r
summarize(dgirt_out_liberalism, pars = "xi", funs = "var")
#>    param year         var
#> 1:    xi 2006 0.024308280
#> 2:    xi 2007 0.006663670
#> 3:    xi 2008 0.005518855
#> 4:    xi 2009 0.004884106
#> 5:    xi 2010 0.005319226
```

To access posterior samples in tabular form use `as.data.frame`. By default, this method returns post-warmup samples for the `theta_bar` parameters, but like other methods takes a `pars` argument.

``` r
head(as.data.frame(dgirt_out_liberalism))
#>        param state race3 year iteration     value
#> 1: theta_bar    CA black 2006         1 0.5239194
#> 2: theta_bar    CA black 2006         2 0.5239194
#> 3: theta_bar    CA black 2006         3 0.5250696
#> 4: theta_bar    CA black 2006         4 0.5250696
#> 5: theta_bar    CA black 2006         5 0.5220594
#> 6: theta_bar    CA black 2006         6 0.5220594
```

To poststratify the results use `poststratify`. The following example uses the group population proportions bundled as `annual_state_race_targets` to reweight and aggregate estimates to strata defined by state-years. Read `help("poststratify")` for more details.

``` r
poststratify(dgirt_out_liberalism, annual_state_race_targets, strata_names = c("state",
    "year"), aggregated_names = "race3")
#>     state year       value
#>  1:    CA 2006  0.14940318
#>  2:    CA 2007  0.16922644
#>  3:    CA 2008  0.12923345
#>  4:    CA 2009  0.07641825
#>  5:    CA 2010  0.09201670
#>  6:    GA 2006  0.08266459
#>  7:    GA 2007  0.01069197
#>  8:    GA 2008 -0.03448823
#>  9:    GA 2009 -0.04094036
#> 10:    GA 2010 -0.01809976
#> 11:    LA 2006 -0.02563313
#> 12:    LA 2007 -0.08929469
#> 13:    LA 2008 -0.12163790
#> 14:    LA 2009 -0.11676727
#> 15:    LA 2010 -0.07733996
#> 16:    MA 2006  0.15940731
#> 17:    MA 2007  0.23502962
#> 18:    MA 2008  0.16406417
#> 19:    MA 2009  0.09824927
#> 20:    MA 2010  0.10487641
```

To plot the results use `dgirt_plot`. This method plots summaries of posterior samples by time period. By default, it shows a 95% credible interval around posterior medians for the `theta_bar` parameters, for each local geographic area. For this (unconverged) toy example we omit the CIs.

``` r
dgirt_plot(dgirt_out_liberalism, y_min = NULL, y_max = NULL)
```

![](README/unnamed-chunk-26-1.png)

`dgirt_plot` can also plot the `data.frame` output from `poststratify`. This requires arguments that identify the relevant variables in the `data.frame`. Below, `poststratify` aggregates over the demographic grouping variable `race3`, resulting in a `data.frame` of estimates by state-year. So, in the subsequent call to `dgirt_plot`, we pass the names of the state and year variables. The `group_names` argument is `NULL` because there are no grouping variables left after aggregating over `race3`.

``` r
ps <- poststratify(dgirt_out_liberalism, annual_state_race_targets, strata_names = c("state",
    "year"), aggregated_names = "race3")
head(ps)
#>    state year      value
#> 1:    CA 2006 0.14940318
#> 2:    CA 2007 0.16922644
#> 3:    CA 2008 0.12923345
#> 4:    CA 2009 0.07641825
#> 5:    CA 2010 0.09201670
#> 6:    GA 2006 0.08266459
dgirt_plot(ps, group_names = NULL, time_name = "year", geo_name = "state")
```

![](README/unnamed-chunk-27-1.png)

Troubleshooting
---------------

Please [report issues](https://github.com/jamesdunham/dgo/issues) that you encounter.

OS X only: RStan creates temporary files during estimation in a location given by `tempdir`, typically an arbitrary location in `/var/folders`. If a model runs for days, these files can be cleaned up while still needed, which induces an error. A good solution is to set a safer path for temporary files, using an environment variable checked at session startup. As described in `?tempdir`,

> The environment variables ‘TMPDIR’, ‘TMP’ and ‘TEMP’ are checked in turn and the first found which points to a writable directory is used: if none succeeds ‘/tmp’ is used. The path should not contain spaces.

For help setting environment variables, see the Stack Overflow question [here](https://stackoverflow.com/questions/17107206/change-temporary-directory). Confirm the new path before starting your model run by restarting R and checking the output from `tempdir()`.

``` r
# Problematic temporary directories on OS X look like this
tempdir()   
#> [1] "/var/folders/2p/_d3c95qd6ljg28j1f5l2jqxm0000gn/T//Rtmp38a10A"
```

Contributing and citing
-----------------------

dgo is under development and we welcome [suggestions](https://github.com/jamesdunham/dgo/issues).

The package citation is

> Dunham, James, Devin Caughey, and Christopher Warshaw. 2017. dgo: Dynamic Estimation of Group-level Opinion. R package. <https://jamesdunham.github.io/dgo/>.
