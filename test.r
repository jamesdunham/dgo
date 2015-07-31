# Test the code for Dynamic IRT
# Created 6/30/15
# J.Dunham
#
# TODO: filestring

rm(list=ls())
setwd("~/projects/group-irt")

library(foreign)        # for reading the test data, FunTestEU150616.dta
library(codetools)      # for interactive use in development

# Read test data
# TODO: use onJames etc. here?
path = '~/Dropbox\ (MIT)/Survey\ Database/Code/'
poll.df = read.dta(paste0(path, 'FunTestEU150616.dta'))

source('dynamicIRT.r')  # Dynamic IRT code

# Read the results of running the original code (all
# objects in the workspace on completion) into their
# own environment.
orig = new.env()
load('original-workspace.Rdata', env = orig)

# Set control options to be used in subsequent function calls
test.opts = list(
  fm.vars = names(select(poll.df, YearFactor:age)),    # front matter variable names (character vector)
  q.vars = names(select(poll.df, -c(YearFactor:age))), # survey question variable names (character vector)
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

# Check the data to be used in the model. For now, don't make any changes; we set test = TRUE.
# TODO: interpret .opts
checkData(.data = poll.df, .opts = test.opts)

# Now let's update the data to be used in the model.
test.opts$test = FALSE
checked.data = checkData(.data = poll.df, .opts = test.opts)

# Examine the resulting data...
# TODO: pull out these as functions
# Create a contingency table of question and year combinations.
checked.data$yrs.asked
# Create a contingency table of question and survey-year combinations.
checked.data$svy.year.asked
# Create a table with counts of the polls in which questions appear.
checked.data$poll.count

# Create the tables and variables that Stan will use
stan.data = formatData(.data = checked.data, .opts = test.opts)

# Take a look
# TODO: implement a summarize() method?
str(stan.data)

# Pass everything to Stan
# TODO: add stan.data and stan.code arguments
# TODO: this is a good place for ...
stan.out = runStan(
  n.iter = 2e3,
  n.chain = 2,
  max.save = 2e3,
  n.warm = min(1e4, floor(n.iter * 3/4)),
  n.thin = ceiling((n.iter - n.warm) / (max.save / n.chain)),
  init.range = 1,
  pars.to.save = c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar",
    "nu_geo", "nu_geo_prior", "kappa", "sd_item",
    "sd_theta", "sd_theta_bar", "sd_gamma",
    "sd_innov_gamma", "sd_innov_delta", "sd_innov_logsd",
    "sd_total", "theta_nat", "var_theta_bar_nat"))
