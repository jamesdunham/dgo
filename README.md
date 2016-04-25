dgirt is an R package for dynamic group-level IRT models as developed in
[Caughey and Warshaw
2014](http://pan.oxfordjournals.org/content/early/2015/02/04/pan.mpu021.full.pdf+html):

> Over the past eight decades, millions of people have been surveyed on
> their political opinions. Until recently, however, polls rarely
> included enough questions in a given domain to apply scaling
> techniques such as IRT models at the individual level, preventing
> scholars from taking full advantage of historical survey data. To
> address this problem, we develop a Bayesian group-level IRT approach
> that models latent traits at the level of demographic and/or
> geographic groups rather than individuals. We use a hierarchical model
> to borrow strength cross-sectionally and dynamic linear models to do
> so across time. The group-level estimates can be weighted to generate
> estimates for geographic units. This framework opens up vast new areas
> of research on historical public opinion, especially at the
> subnational level.

Quick start
===========

    devtools::install_github("jamesdunham/dgirt")
    library(dgirt)

    # shape item response data for modeling
    dgirt_in <- shape(state_opinion, item_names = "Q_cces2006_minimumwage",
                      time_name = "year", geo_name = "state", group_names = "race",
                      time_filter = 2006:2008, geo_filter = c("MA", "NY"),
                      survey_name = "source", weight_name = "weight")

    ## Applying restrictions, pass 1...
    ##  Dropped 5 rows for missingness in covariates
    ##  Dropped 254 rows for lacking item responses
    ## Applying restrictions, pass 2...
    ##  No changes
    ## 
    ## item                               class      levels       responses
    ## --------------------------------------------------------------------
    ## Q_cces2006_minimumwage           integer           2           5,274
    ## --------------------------------------------------------------------

    # inspect data
    summary(dgirt_in)

    ## Items:
    ## [1] "Q_cces2006_minimumwage"
    ## Respondents:
    ##    5,274 in `item_data` (unadjusted)
    ## Grouping variables:
    ## [1] "year"  "state" "race" 
    ## Time periods:
    ## [1] 2006 2007 2008
    ## Local geographic areas:
    ## [1] "MA" "NY"
    ## Hierarchical parameters:
    ## [1] "stateNY"   "raceother" "racewhite"
    ## Hierarchical parameters with modifiers:
    ## character(0)
    ## Constants:
    ##  Q  T  P  N  G  H  D 
    ##  1  3  3 18  6  1  1

    get_item_n(dgirt_in, by = "year")

    ##    year Q_cces2006_minimumwage
    ## 1: 2006                   2220
    ## 2: 2007                    887
    ## 3: 2008                   2167

    get_n(dgirt_in, by = c("year", "source"))

    ##    year    source    n
    ## 1: 2006 CCES_2006 2220
    ## 2: 2007 CCES_2007  887
    ## 3: 2008 CCES_2008 2167

    # fit model
    dgirt_out <- dgirt(dgirt_in, iter = 2000, chains = 2, cores = 2, seed = 42)

    # verbose output omitted

All `rstan` methods for the `stanfit` class are available for `dgirt`
output, so do things the `rstan` way. Descriptive labels for parameters
on time periods, local geographic areas, and grouping variables will be
added to most output.

    # examine results
    summary(dgirt_out, pars = "theta_bar", probs = NULL)[[1]][, c("n_eff", "Rhat")]

    ##                              n_eff     Rhat
    ## theta_bar[MA__black,2006] 567.2511 1.001307
    ## theta_bar[NY__other,2006] 672.1604 1.000926
    ## theta_bar[MA__black,2006] 632.3718 1.000982
    ## theta_bar[NY__other,2006] 562.4402 1.001778
    ## theta_bar[MA__black,2006] 586.9589 1.002445
    ## theta_bar[NY__other,2006] 577.8566 1.001134
    ## theta_bar[NY__black,2007] 217.2885 1.009635
    ## theta_bar[MA__white,2007] 242.3918 1.010424
    ## theta_bar[NY__black,2007] 231.9694 1.010861
    ## theta_bar[MA__white,2007] 213.2866 1.011052
    ## theta_bar[NY__black,2007] 207.3987 1.011877
    ## theta_bar[MA__white,2007] 202.3302 1.012442
    ## theta_bar[MA__other,2008] 230.2633 1.011293
    ## theta_bar[NY__white,2008] 294.0922 1.008538
    ## theta_bar[MA__other,2008] 249.6917 1.010232
    ## theta_bar[NY__white,2008] 236.7750 1.011143
    ## theta_bar[MA__other,2008] 261.8406 1.009593
    ## theta_bar[NY__white,2008] 245.5359 1.010356

    # get posterior means with a convenience function
    get_posterior_mean(dgirt_out, pars = "theta_bar")

    ##                           mean-chain:1 mean-chain:2 mean-all chains
    ## theta_bar[MA__black,2006]    14.223879    13.732237       13.978058
    ## theta_bar[NY__black,2007]     7.062407     6.700662        6.881535
    ## theta_bar[MA__other,2008]     7.982472     7.790585        7.886528
    ## theta_bar[NY__other,2006]    14.846393    14.496723       14.671558
    ## theta_bar[MA__white,2007]     9.209964     9.046366        9.128165
    ## theta_bar[NY__white,2008]     8.064513     7.787008        7.925760
    ## theta_bar[MA__black,2006]    32.711052    36.788709       34.749881
    ## theta_bar[NY__black,2007]    19.957765    22.340277       21.149021
    ## theta_bar[MA__other,2008]    23.458089    26.078331       24.768210
    ## theta_bar[NY__other,2006]    33.676353    37.582956       35.629654
    ## theta_bar[MA__white,2007]    21.293037    24.110654       22.701845
    ## theta_bar[NY__white,2008]    19.084285    21.753493       20.418889
    ## theta_bar[MA__black,2006]   139.191189   184.742037      161.966613
    ## theta_bar[NY__black,2007]    88.571519   115.378657      101.975088
    ## theta_bar[MA__other,2008]    95.441928   123.841169      109.641548
    ## theta_bar[NY__other,2006]   143.635484   189.234568      166.435026
    ## theta_bar[MA__white,2007]    97.492839   126.812593      112.152716
    ## theta_bar[NY__white,2008]    91.263702   119.676549      105.470125

    # access posterior samples
    apply(as.array(dgirt_out, pars = "theta_bar"), 3, mean)

    ## theta_bar[MA__black,2006] theta_bar[NY__black,2007] 
    ##                 13.978058                 34.749881 
    ## theta_bar[MA__other,2008] theta_bar[NY__other,2006] 
    ##                161.966613                  6.881535 
    ## theta_bar[MA__white,2007] theta_bar[NY__white,2008] 
    ##                 21.149021                101.975088 
    ## theta_bar[MA__black,2006] theta_bar[NY__black,2007] 
    ##                  7.886528                 24.768210 
    ## theta_bar[MA__other,2008] theta_bar[NY__other,2006] 
    ##                109.641548                 14.671558 
    ## theta_bar[MA__white,2007] theta_bar[NY__white,2008] 
    ##                 35.629654                166.435026 
    ## theta_bar[MA__black,2006] theta_bar[NY__black,2007] 
    ##                  9.128165                 22.701845 
    ## theta_bar[MA__other,2008] theta_bar[NY__other,2006] 
    ##                112.152716                  7.925760 
    ## theta_bar[MA__white,2007] theta_bar[NY__white,2008] 
    ##                 20.418889                105.470125

See `help("dgirtfit-class")` and help`("stanfit-class")` for more.
