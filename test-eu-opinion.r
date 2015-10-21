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

# Read EU test data
eu.df = read.dta('FunTestEU150616.dta')
glimpse(eu.df)

# Set control options to be used in subsequent function calls, for EU test data
eu.test.opts = list(
  fm.vars = names(select(eu.df, YearFactor:age)),    # front matter variable names (character vector)
  q.vars = names(select(eu.df, -c(YearFactor:age))), # survey question variable names (character vector)
  t.var = 'YearFactor',             # time variable (character)
  use.t = levels(eu.df$YearFactor),
  # TODO: replace 'years' with 'periods'/'time' throughout; we're agnostic about units
  min.t = 1L,                         # questions appearing in any fewer periods will be dropped (numeric)
  min.poll = 1L,                       # questions appearing in any fewer polls will be dropped (numeric)
  poll.var = 'survey',
  geo.var = 'Country',                 # geographic variable (character)
  demo.vars = 'Gender',                # demographic variables (character vector)
  geo.mod.vars = NULL,                 # TODO: description (character vector)
  geo.mod.prior.vars = NULL,           # TODO: description (character vector)
  weight.var = 'useweight',            # weight variable (character)
  # TODO: just use T/F
  separate.t = as.integer(FALSE),      # no smoothing over time? (logical)
  constant.item = as.integer(TRUE),    # make difficulty parameters constant over time? (logical)
  test = TRUE)                         # run checks without touching the data? (logical)

# Check the data to be used in the model. For now, don't make any changes; we set test = TRUE.
# checkData(.data = eu.df, .opts = eu.test.opts)

# Now let's update the data to be used in the model.
eu.test.opts$test = FALSE
eu.checked.data = checkData(.data = eu.df, .opts = eu.test.opts)

# Examine the resulting data...
# TODO: pull out these as functions
# Print a contingency table of question and year combinations.
eu.checked.data$summary$q.when.asked
# Print a contingency table of question and survey-year combinations.
eu.checked.data$summary$q.which.observed
# Print counts of the polls in which questions appear.
t(eu.checked.data$summary$q.counts)

# Create the tables and variables that Stan will use
eu.stan.data = formatData(.data = eu.checked.data, .opts = eu.test.opts)

# Take a look
# TODO: implement a summarize() method?
str(eu.stan.data)
str(eu.checked.data$varnames)

# Number of level-one responses (should be equal)
length(eu.stan.data$n_vec)
length(eu.stan.data$s_vec)

# Periods
eu.stan.data$T
levels(unlist(eu.checked.data$data[eu.checked.data$varnames$t.var]))

# Questions
eu.stan.data$Q

# Covariate groups
eu.stan.data$G

levels(unlist(eu.checked.data$data[eu.checked.data$varnames$geo.var]))

# Covariates
dim(eu.stan.data$ZZ)
sum(eu.stan.data$ZZ)
sum(eu.stan.data$ZZ_prior)

# Level-two covariate groups
sum(eu.stan.data$NNl2)
sum(eu.stan.data$SSl2)
sum(eu.stan.data$l2_only)

# Missingness
dim(eu.stan.data$MMM)
sum(eu.stan.data$MMM) / length(eu.stan.data$MMM)

## Test package output against original script's output ##

# Read the results of running the original code (all objects in the workspace
# on completion) into their own environment.
eu.orig = new.env()
load('original-eu-output.Rdata', env = eu.orig)
eu.orig.data = get('stan.data', envir = eu.orig)

# names of elements in the package and original stan.data should match, except
# 'nat' -> 'l2
setdiff(names(eu.stan.data), names(eu.orig.data))
check.ns.vecs = Reduce(full_join, list(
  data.frame(cell = names(eu.orig.data$n_vec), n.orig = eu.orig.data$n_vec),
  data.frame(cell = names(eu.stan.data$n_vec), n.current = eu.stan.data$n_vec),
  data.frame(cell = names(eu.orig.data$s_vec), s.orig = eu.orig.data$s_vec),
  data.frame(cell = names(eu.stan.data$s_vec), s.current = eu.stan.data$s_vec))) %>%
  arrange(cell) %>%
  mutate(
    # The current code records 0 for unobserved variable combinations in n_vec
    n.orig = ifelse(is.na(n.orig), 0, n.orig),
    s.orig = ifelse(is.na(s.orig), 0, s.orig),
    n.match = n.orig == n.current,
    s.match = s.orig == s.current) %>%
  as.tbl()
check.ns.vecs %>% filter(!n.match) %>% select(-contains('match'))
check.ns.vecs %>% filter(!s.match) %>% select(-contains('match'))

# Identical
identical(eu.orig.data$ZZ.prior, eu.stan.data$ZZ.prior)
identical(eu.orig.data$Q, eu.stan.data$Q)
identical(eu.orig.data$T, eu.stan.data$T)
identical(eu.orig.data$H, eu.stan.data$H)
identical(eu.orig.data$Hprior, eu.stan.data$Hprior)
identical(eu.orig.data$separate_years, eu.stan.data$separate_years)
identical(eu.orig.data$constant_item, eu.stan.data$constant_item)
identical(eu.orig.data$D, eu.stan.data$D)
identical(eu.orig.data$G, eu.stan.data$G)
identical(eu.orig.data$P, eu.stan.data$P)
identical(eu.orig.data$S, eu.stan.data$S)

# Meaningfuly not identical
identical(eu.orig.data$WT, eu.stan.data$WT)
str(eu.stan.data$WT)
str(eu.orig.data$WT)
# NOTE: This structure doesn't make sense to me
eu.orig.data$WT[, 1, ]
eu.orig.data$WT[, 2, ]

# Not identical, but all equal
identical(eu.orig.data$XX, eu.stan.data$XX)
sum(eu.orig.data$XX != eu.stan.data$XX)
identical(eu.orig.data$nat_only, eu.stan.data$nat_only)
sum(eu.orig.data$nat_only != eu.stan.data$nat_only)
identical(eu.orig.data$Gnat, eu.stan.data$Gnat)

# All equal, but not identical; in NNnat and SSnat, the order of Male and
# Female are switched
all(eu.orig.data$NNnat == eu.stan.data$NNl2)
all(eu.orig.data$SSnat == eu.stan.data$SSl2)
identical(eu.orig.data$NNnat, eu.stan.data$NNl2)
identical(eu.orig.data$SSnat, eu.stan.data$SSl2)
str(eu.orig.data$NNnat)
str(eu.stan.data$NNl2)
all(eu.orig.data$ZZ == eu.stan.data$ZZ)

# NOTE: we don't have dimnames in the original MMM here
# FIXME: we had a question dimension in the original missingness array, now
# omitted
dim(eu.orig.data$MMM)
dim(eu.stan.data$MMM)
identical(dim(eu.orig.data$MMM), dim(eu.stan.data$MMM))
identical(table(eu.orig.data$MMM, useNA = 'always'), table(eu.stan.data$MMM, useNA = 'always'))

# FIXME: not all s_vec values match; try to fix this last, in case it's a
# sorting issue that's cascading from above
length(eu.orig.data$s_vec)
length(eu.stan.data$s_vec)
table(eu.orig.data$s_vec == eu.stan.data$s_vec, useNA = 'always')

length(eu.orig.data$n_vec)
length(eu.stan.data$n_vec)
table(eu.orig.data$n_vec == eu.stan.data$n_vec, useNA = 'always')

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
stan.out <- runStan(
  stan.code = stan.code,
  stan.data = eu.stan.data,
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
