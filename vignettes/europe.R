# Constants
country_set <- "strictwestern"
issue_domain <- "immi"
item_model <- "constant"
time_name <- "biennium"
geo_name <- "country"
group_names <- "gender_age"


# NOTE: If the mgirt branch is installed, load it like this
library(dgo)
# Otherwise, point load_all to the path where the mgirt branch is downloaded,
# like
devtools::load_all('~/dev/dgo')
stopifnot(packageVersion('dgo') >= '0.3.0')

library(tidyverse)
library(reshape2)
library(rstan)
library(assertthat)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
sessionInfo()

# Don't recompile the model if we've already compiled and saved it in a previous
# session
if (file.exists('model.Rds')) {
  model = readRDS('model.Rds')
} else {
  model = NULL
}

# Define some functions we'll use to prepare the data. Some of these might move into dgo.

years_to_biennia <- function (years, odd_start = TRUE) {
  if (odd_start) {
    paste(trunc((years - 1) / 2) * 2 + 1, trunc((years - 1) / 2) * 2 + 2,
      sep = "-")
  } else {
    paste(trunc(years / 2) * 2, trunc(years / 2) * 2 + 1, sep = "-")
  }
}

#### LOAD DATA
load("~/Dropbox (MIT)/dgo/europe_example/180731_immidata.Rda")
opin_wide = immi_data

item_vars <- names(select(opin_wide, -c(survey:w_europe)))
#  [1] "immbetter"   "immcult"     "immgood"     "immpoor"     "immdiff"    
#  [6] "immsame"     "legalrights" "immprove"    "takejobs"    "immecon"    
# [11] "immcrime"    "trads"       "imports"     "concimms"    "limitfors"  
# [16] "scarceimms"  "allhome"     "sendall"     "sendump"     "socright"   
# [21] "forinfl"    

biennia_to_est <- seq.int(min(opin_wide$year), max(opin_wide$year)) %>%
  years_to_biennia(odd_start = TRUE) %>%
  unique()
biennia_to_est
#  [1] "1989-1990" "1991-1992" "1993-1994" "1995-1996" "1997-1998"
#  [6] "1999-2000" "2001-2002" "2003-2004" "2005-2006" "2007-2008"
# [11] "2009-2010" "2011-2012" "2013-2014" "2015-2016"

opin_wide <- opin_wide %>%
  mutate(
    # Coarsen year
    biennium = factor(years_to_biennia(year, odd_start = TRUE),
      levels = biennia_to_est),
    country = droplevels(country),
    country = factor(country, labels = gsub("\\_", " ", levels(country))),
    # Create an age categorical
    age3 = case_when(
      sixteen == 1 ~ "aged 16-34",
      thirtyfive == 1 ~ "aged 35-59",
      sixty == 1 ~ "aged 60+"
      ),
    age3 = factor(age3),
    gender = factor(gender + 1, labels = c("male", "female")),
    gender_age = interaction(gender, age3, sep = " | "))

summary(opin_wide)

# Confirm translation to biennia worked
table(opin_wide$biennium, opin_wide$year)

# Create Stan inputs.

# # TODO: why?
# if (packageVersion('rstan') >= '2.18') {
#   Sys.setenv(USE_CXX14 = 1)
# }
# stan_mod <- rstan::stan_model(model_code = mdgirt_code)

stan_data = shape_multinomial(opin_wide,
  item_names = item_vars, 
  time_name = time_name,
  geo_name = geo_name,
  group_names = group_names,
  weight_name = 'weight')

fit = mgirt(stan_data, iter = 10, model = model)


