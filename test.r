# Test the DIRT code
# Created 6/30/15
# J.Dunham
#
# Attempt to fit the DIRT model using two datasets. First, use the EU opinion
# data (eu.df), allowing comparisons between the output of the current DIRT
# code and that from original, project-specific DIRT code. Second, use the abortion opinion data compared to
# and abortion opinion

rm(list=ls())

library(foreign)        # for reading the EU test data, FunTestEU150616.dta
library(codetools)      # for interactive use in development

# Set working directory
if (grepl("devin", Sys.info()["user"], ignore.case=TRUE)) {
  setwd("~/git/group-irt")
} else {
  setwd('~/projects/group-irt')
}

# Read EU test data
# NOTE: [JD 9/3] moved EU and abortion data to git folder
eu.df = read.dta('FunTestEU150616.dta')
# Read abortion test data:
#   * abort.df: individual-level data
#   * abort.st.df: state-level (group) data
#   * abort.fm.vars: names of frontmatter variables
#   * abort.q.vars: names of question variables
load('abortion.test.data.Rdata')

source('dynamicIRT.r')  # Group IRT code

# Read the results of running the original code (all objects in the workspace
# on completion) into their own environment.
eu.orig = new.env()
load('original-eu-output.Rdata', env = eu.orig)

# Set control options to be used in subsequent function calls, for EU test data
eu.test.opts = list(
  fm.vars = names(select(eu.df, YearFactor:age)),    # front matter variable names (character vector)
  q.vars = names(select(eu.df, -c(YearFactor:age))), # survey question variable names (character vector)
  time.var = 'YearFactor',             # time variable (character)
  # TODO: replace 'years' with 'periods'/'time' throughout; we're agnostic about units
  min.years = 1L,                      # questions appearing in any fewer periods will be dropped (numeric)
  min.polls = 1L,                      # questions appearing in any fewer polls will be dropped (numeric)
  geo.var = 'Country',                 # geographic variable (character)
  demo.vars = 'Gender',                # demographic variables (character vector)
  geo.mod.vars = NULL,                 # TODO: description (character vector)
  geo.mod.prior.vars = NULL,           # TODO: description (character vector)
  # TODO: just use T/F
  separate.years = as.integer(FALSE),  # no smoothing over time? (logical)
  constant.item = as.integer(TRUE),    # make difficulty parameters constant over time? (logical)
  test = TRUE)                         # run checks without touching the data? (logical)

# Do the same for abortion test data
abort.test.opts = list(
  fm.vars = abort.fm.vars,            # front matter variable names (character vector)
  q.vars = abort.q.vars,              # survey question variable names (character vector)
  time.var = 'Year',                  # time variable (character)
  # TODO: replace 'years' with 'periods'/'time' throughout; we're agnostic about units
  min.years = 1L,                      # questions appearing in any fewer periods will be dropped (numeric)
  min.polls = 1L,                      # questions appearing in any fewer polls will be dropped (numeric)
  geo.var = 'StPOAbrv',                 # geographic variable (character)
  demo.vars = 'Gender',                # demographic variables (character vector)
  geo.mod.vars = NULL,                 # TODO: description (character vector)
  geo.mod.prior.vars = NULL,           # TODO: description (character vector)
  # TODO: just use T/F
  separate.years = as.integer(FALSE),  # no smoothing over time? (logical)
  constant.item = as.integer(TRUE),    # make difficulty parameters constant over time? (logical)
  test = TRUE)                         # run checks without touching the data? (logical)

# Check the data to be used in the model. For now, don't make any changes; we set test = TRUE.
# TODO: interpret .opts
checkData(.data = eu.df, .opts = eu.test.opts)
checkData(.data = abort.df, .opts = abort.test.opts)

# Now let's update the data to be used in the model.
eu.test.opts$test = FALSE
checked.data = checkData(.data = eu.df, .opts = eu.test.opts)

# Examine the resulting data...
# TODO: pull out these as functions
# Create a contingency table of question and year combinations.
checked.data$yrs.asked
# Create a contingency table of question and survey-year combinations.
checked.data$svy.year.asked
# Create a table with counts of the polls in which questions appear.
checked.data$poll.count

# Create the tables and variables that Stan will use
stan.data = formatData(.data = checked.data, .opts = eu.test.opts)

# Take a look
# TODO: implement a summarize() method?
str(stan.data)

save(stan.data, file = paste0('pkg.stan.data.', as.Date(Sys.time()), '.Rdata'))
eu.orig.data = get('stan.data', envir = eu.orig)

## Test package output against original script's output ##

# names of elements in the package and original stan.data should match
stopifnot(length(setdiff(names(stan.data), names(eu.orig.data))) == 0)

# All identical
identical(stan.data$n_vec[sort(names(stan.data$n_vec))],
  eu.orig.data$n_vec[sort(names(eu.orig.data$n_vec))])
identical(eu.orig.data$XX, stan.data$XX)
identical(eu.orig.data$ZZ.prior, stan.data$ZZ.prior)
identical(eu.orig.data$nat_only, stan.data$nat_only)
identical(eu.orig.data$G, stan.data$G)
identical(eu.orig.data$Q, stan.data$Q)
identical(eu.orig.data$T, stan.data$T)
identical(eu.orig.data$N, unname(stan.data$N))
identical(eu.orig.data$P, stan.data$P)
identical(eu.orig.data$S, stan.data$S)
identical(eu.orig.data$H, stan.data$H)
identical(eu.orig.data$Hprior, stan.data$Hprior)
identical(eu.orig.data$separate_years, stan.data$separate_years)
identical(eu.orig.data$constant_item, stan.data$constant_item)
identical(eu.orig.data$D, stan.data$D)
identical(eu.orig.data$WT, stan.data$WT)
identical(eu.orig.data$Gnat, stan.data$Gnat)

# All equal, but not identical; in NNnat and SSnat, the order of Male and
# Female are switched
all(eu.orig.data$NNnat == stan.data$NNnat)
all(eu.orig.data$SSnat == stan.data$SSnat)
identical(eu.orig.data$NNnat, stan.data$NNnat)
identical(eu.orig.data$SSnat, stan.data$SSnat)
str(eu.orig.data$NNnat)
str(stan.data$NNnat)
all(eu.orig.data$ZZ == stan.data$ZZ)


# FIXME: MMM has matching table()s but isn't equal pairwise. Is this a sorting
# problem?
# NOTE: we don't have dimnames in the original MMM here
all(eu.orig.data$MMM == unname(stan.data$MMM))
identical(table(eu.orig.data$MMM), table(stan.data$MMM))
str(eu.orig.data$MMM)
str(stan.data$MMM)

# FIXME: not all s_vec values match; try to fix this last, in case it's a
# sorting issue that's cascading from above
compare.s_vec =
  stan.data$s_vec[sort(names(stan.data$s_vec))] ==
  eu.orig.data$s_vec[sort(names(eu.orig.data$s_vec))]
table(compare.s_vec)
# For example:
# Original                                   Package
# Austria__Female__2008 | freefirms.gt1 14   freefirms.gt1 14
# Austria__Female__2008 | freefirms.gt2 11   freefirms.gt2 14
# Austria__Female__2008 | freefirms.gt3 10   freefirms.gt3 12
# Austria__Female__2008 | freefirms.gt4 8    freefirms.gt4 11
# Austria__Female__2008 | freefirms.gt5 7    freefirms.gt5 9
# Austria__Female__2008 | freefirms.gt6 4    freefirms.gt6 6
# Austria__Female__2008 | freefirms.gt7 3    freefirms.gt7 5
# Austria__Female__2008 | freefirms.gt8 1    freefirms.gt8 3
# Austria__Female__2008 | freefirms.gt9 2    freefirms.gt9 0

## Fit model ##

# Pass everything to Stan
# TODO: this is a good place for ... argument
stan.out <- runStan(
    stan.code = stan.code,
    stan.data = stan.data,
    n.iter = 2e1,
    n.chain = 2,
    max.save = 2e3,
    init.range = 1,
    seed = 1,
    pars.to.save = c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
        "nu_geo", "nu_geo_prior", "kappa", "sd_item",
        "sd_theta", "sd_theta_bar", "sd_gamma",
        "sd_innov_gamma", "sd_innov_delta", "sd_innov_logsd",
        "sd_total", "theta_nat", "var_theta_bar_nat"),
    parallel = TRUE)
