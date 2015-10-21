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

source('dynamicIRT.r')  # Group IRT code

# Read abortion test data:
#   * abort.df: individual-level data
#   * abort.st.df: state-level (group) data
#   * abort.fm.vars: names of frontmatter variables
#   * abort.q.vars: names of question variables
load('abortion.test.data.Rdata')
abort.q.vars = abort.q.vars[-grep("binary", abort.q.vars)]

# Set control options to be used in subsequent function calls
abort.test.opts = list(
  fm.vars = abort.fm.vars,            # front matter variable names (character vector)
  q.vars = abort.q.vars,              # survey question variable names (character vector)
  t.var = 'year',                     # time variable (character)
  use.t = 1972:2012,                  # a vector t_min:t_max
  # TODO: replace 'years' with 'periods'/'time' throughout; we're agnostic about units
  min.t = 1L,                         # questions appearing in any fewer periods will be dropped (numeric)
  min.poll = 1L,                      # questions appearing in any fewer polls will be dropped (numeric)
  poll.var = "source",                # survey identifier
  geo.var = 'StPOAbrv',               # geographic variable (character)
  demo.vars = 'Female',               # demographic variables (character vector)
  geo.mod.vars = NULL,                # TODO: description (character vector)
  geo.mod.prior.vars = NULL,          # TODO: description (character vector)
  weight.var = 'weight',              # weight variable (character)
  # TODO: just use T/F
  separate.t = as.integer(FALSE),  # no smoothing over time? (logical)
  constant.item = as.integer(TRUE),    # make difficulty parameters constant over time? (logical)
  test = TRUE)                         # run checks without touching the data? (logical)

# Check the data to be used in the model. For now, don't make any changes; we set test = TRUE.
# checkData(.data = abort.df, .opts = abort.test.opts)

# Now let's update the data to be used in the model.
abort.test.opts$test = FALSE
abort.checked.data = checkData(.data = abort.df, .opts = abort.test.opts)

# Examine the resulting data...
# TODO: pull out these as functions
# Print a contingency table of question and year combinations.
abort.checked.data$summary$q.when.asked
# Print a contingency table of question and survey-year combinations.
abort.checked.data$summary$q.which.observed
# Print counts of the polls in which questions appear.
# TODO: this should be transposed for readability
t(abort.checked.data$summary$q.counts)

# Create the tables and variables that Stan will use
abort.stan.data = formatData(.data = abort.checked.data, .opts = abort.test.opts)

# Take a look
# TODO: implement a summarize() method?
str(abort.stan.data)

## Fit model ##

# Pass everything to Stan
# TODO: this is a good place for ... argument
stan.out <- runStan(
  stan.code = stan.code,
  stan.data = abort.stan.data,
  n.iter = 2e1,
  n.chain = 2,
  max.save = 2e3,
  init.range = 1,
  seed = 1,
  pars.to.save = c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
    "nu_geo", "nu_geo_prior", "kappa", "sd_item",
    "sd_theta", "sd_theta_bar", "sd_gamma",
    "sd_innov_gamma", "sd_innov_delta", "sd_innov_logsd",
    "sd_total", "theta_l2", "var_theta_bar_l2"),
  parallel = TRUE)

