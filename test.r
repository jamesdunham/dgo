# Test the code for Dynamic IRT
# Created 6/30/15
# J.Dunham
#
# TODO: filestring

rm(list=ls())

onDevin <- function (...) {
  user <- Sys.info()["user"]
  onD <- grepl("devin", user, ignore.case=TRUE) 
  return(onD)
}

if (onDevin()) {
  setwd("~/git/group-irt")
} else {
  setwd('~/projects/group-irt')
}

library(foreign)        # for reading the test data, FunTestEU150616.dta
library(codetools)      # for interactive use in development

# Read test data
# TODO: use onJames etc. here?
if (onDevin()) {
  path <- "~/Dropbox (Personal)/2-Shared/Work/RAs/Dunham/Survey Database/Code/"
} else {
  path = '~/Dropbox\ (MIT)/Survey\ Database/Code/'
}
poll.df = read.dta(paste0(path, 'FunTestEU150616.dta'))

source('dynamicIRT.r')  # Dynamic IRT code

# Read the results of running the original code (all objects in the workspace
# on completion) into their own environment.
orig = new.env()
### DC: This doesn't work on my computer
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

save(stan.data, file = paste0('pkg.stan.data.', as.Date(Sys.time()), '.Rdata'))
orig.data = get('stan.data', envir = orig)

## Test package output against original script's output ##

# names of elements in the package and original stan.data should match
stopifnot(length(setdiff(names(stan.data), names(orig.data))) == 0)

# All identical
identical(stan.data$n_vec[sort(names(stan.data$n_vec))],
  orig.data$n_vec[sort(names(orig.data$n_vec))])
identical(orig.data$XX, stan.data$XX)
identical(orig.data$ZZ.prior, stan.data$ZZ.prior)
identical(orig.data$nat_only, stan.data$nat_only)
identical(orig.data$G, stan.data$G)
identical(orig.data$Q, stan.data$Q)
identical(orig.data$T, stan.data$T)
identical(orig.data$N, unname(stan.data$N))
identical(orig.data$P, stan.data$P)
identical(orig.data$S, stan.data$S)
identical(orig.data$H, stan.data$H)
identical(orig.data$Hprior, stan.data$Hprior)
identical(orig.data$separate_years, stan.data$separate_years)
identical(orig.data$constant_item, stan.data$constant_item)
identical(orig.data$D, stan.data$D)
identical(orig.data$WT, stan.data$WT)
identical(orig.data$Gnat, stan.data$Gnat)

# All equal, but not identical; in NNnat and SSnat, the order of Male and
# Female are switched
all(orig.data$NNnat == stan.data$NNnat)
all(orig.data$SSnat == stan.data$SSnat)
identical(orig.data$NNnat, stan.data$NNnat)
identical(orig.data$SSnat, stan.data$SSnat)
str(orig.data$NNnat) 
str(stan.data$NNnat)
all(orig.data$ZZ == stan.data$ZZ)


# FIXME: MMM has matching table()s but isn't equal pairwise. Is this a sorting
# problem?
# NOTE: we don't have dimnames in the original MMM here
all(orig.data$MMM == unname(stan.data$MMM))
identical(table(orig.data$MMM), table(stan.data$MMM))
str(orig.data$MMM)
str(stan.data$MMM)

# FIXME: not all s_vec values match; try to fix this last, in case it's a
# sorting issue that's cascading from above
compare.s_vec =
  stan.data$s_vec[sort(names(stan.data$s_vec))] ==
  orig.data$s_vec[sort(names(orig.data$s_vec))]
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
