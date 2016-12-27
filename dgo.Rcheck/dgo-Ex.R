pkgname <- "dgo"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('dgo')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("dgirtfit-class")
### * dgirtfit-class

flush(stderr()); flush(stdout())

### Name: dgirtfit-class
### Title: Class 'dgirtfit': a class for fitted DGIRT modelsf
### Aliases: dgirtfit dgirtfit-class

### ** Examples

data(toy_dgirtfit)
# summarize the fitted results
summary(toy_dgirtfit, pars = 'xi')

# get posterior means with a convenience function
get_posterior_mean(toy_dgirtfit, pars = 'theta_bar')

# generally apply functions to posterior samples after warmup; n.b.
# `as.array` is iterations x chains x parameters so `MARGIN = 3` applies
# `FUN` over iterations and chains
apply(as.array(toy_dgirtfit, pars = 'xi'), 3, mean)

# access the posterior samples
as.array(toy_dgirtfit, pars = 'theta_bar')
as.data.frame(toy_dgirtfit, pars = 'theta_bar')
extract(toy_dgirtfit, pars = 'theta_bar')



cleanEx()
nameEx("dgirtfit-methods")
### * dgirtfit-methods

flush(stderr()); flush(stdout())

### Name: show,dgirtfit-method
### Title: 'print' method for 'dgirtfit-class' objects
### Aliases: as.data.frame.dgirtfit get_elapsed_time,dgirtfit-method
###   get_posterior_mean,dgirtfit-method print,dgirtfit-method
###   print.dgirtfit rhats rhats,dgirtfit-method show,dgirtfit-method
###   summarize summarize,dgirtfit-method summary,dgirtfit-method

### ** Examples

# access posterior samples
as.data.frame(toy_dgirtfit, pars = 'theta_bar')
rhats(toy_dgirtfit)



cleanEx()
nameEx("dgirtin-class")
### * dgirtin-class

flush(stderr()); flush(stdout())

### Name: dgirtin-class
### Title: Class 'dgirtIn': data prepared for modeling with 'dgirt'
### Aliases: dgirtIn-method, dgirtin-class dgirtin-class, get_item_n
###   get_item_n, get_item_n,dgirtIn-method get_item_names get_item_names,
###   get_item_names,dgirtIn-method get_n get_n, get_n,dgirtIn-method print
###   print,dgirtIn-method print.dgirtIn, show,dgirtIn-method summary
###   summary,dgirtIn-method

### ** Examples

get_item_names(toy_dgirt_in)
get_n(toy_dgirt_in)
get_n(toy_dgirt_in, by = "year")
get_n(toy_dgirt_in, by = "source")
get_item_n(toy_dgirt_in)
get_item_n(toy_dgirt_in, by = "year")
get_item_names(toy_dgirt_in)
# respondent count
get_n(toy_dgirt_in)

# respondent count by year
get_n(toy_dgirt_in, by = "year")

# respondent count by survey identifier
get_n(toy_dgirt_in, by = "source")

get_item_n(toy_dgirt_in)
get_item_n(toy_dgirt_in, by = "year")



cleanEx()
nameEx("opinion")
### * opinion

flush(stderr()); flush(stdout())

### Name: opinion
### Title: 'dgirt' example data: item responses
### Aliases: opinion

### ** Examples

opinion



cleanEx()
nameEx("plot-method")
### * plot-method

flush(stderr()); flush(stdout())

### Name: dgirt_plot
### Title: 'dgirt_plot': plot 'dgirtfit'-class objects
### Aliases: dgirt_plot dgirt_plot,data.frame-method
###   dgirt_plot,dgirtfit-method plot,dgirtfit,missing-method plot_rhats
###   plot_rhats,dgirtfit-method

### ** Examples

dgirt_plot(toy_dgirtfit)
dgirt_plot(toy_dgirtfit, y_min = NULL, y_max = NULL)
p <- dgirt_plot(toy_dgirtfit)
p %+% ylab("posterior median")
data(state_year_targets)
ps <- poststratify(toy_dgirtfit, state_year_targets, strata_names =
                   c("state", "year"), aggregated_names = "race")
dgirt_plot(ps, group_names = NULL, time_name = "year", geo_name = "state")

plot(toy_dgirtfit)
plot_rhats(toy_dgirtfit)
plot_rhats(toy_dgirtfit, facet_vars = c("race", "state")) +
  scale_x_continuous(breaks = seq.int(2006, 2008))



cleanEx()
nameEx("poststratify")
### * poststratify

flush(stderr()); flush(stdout())

### Name: poststratify
### Title: 'poststratify': reweight and aggregate estimates
### Aliases: poststratify poststratify,data.frame-method
###   poststratify,dgirtfit-method

### ** Examples


data(toy_dgirtfit)

# the stratifying variables should uniquely identify proportions in the
# target data; to achieve this, sum over the other variables
targets <- aggregate(proportion ~ state + year + race, targets, sum)

# the dgirtfit method of poststratify takes a dgirtfit object, the target
# data, the names of variables that define population strata, and the  names
# of variables to be aggregated over
post <- poststratify(toy_dgirtfit, targets, c("state", "year"), "race")



cleanEx()
nameEx("shape")
### * shape

flush(stderr()); flush(stdout())

### Name: shape
### Title: 'shape': prepare data for modeling with 'dgirt'
### Aliases: shape

### ** Examples

# model individual item responses
data(opinion)
opinion$respondent = 1:nrow(opinion)
shaped_responses <- shape(opinion,
                          item_names = "Q_cces2006_gaymarriageamendment",
                          time_name = "year",
                          geo_name = "state",
                          group_names = "race",
                          weight_name = "weight",
                          survey_name = "source",
                          id_vars = 'respondent')
# summarize result)
summary(shaped_responses)

# check sparseness of data to be modeled
get_item_n(shaped_responses, by = "year")




cleanEx()
nameEx("state_year_targets")
### * state_year_targets

flush(stderr()); flush(stdout())

### Name: state_year_targets
### Title: 'dgirt' example data: state-year population targets
### Aliases: state_year_targets

### ** Examples

head(state_year_targets)



cleanEx()
nameEx("states")
### * states

flush(stderr()); flush(stdout())

### Name: states
### Title: 'dgirt' example data: state demographics
### Aliases: states

### ** Examples

states



cleanEx()
nameEx("targets")
### * targets

flush(stderr()); flush(stdout())

### Name: targets
### Title: 'dgirt' example data: U.S. population targets
### Aliases: targets

### ** Examples

targets



cleanEx()
nameEx("toy_dgirt_in")
### * toy_dgirt_in

flush(stderr()); flush(stdout())

### Name: toy_dgirt_in
### Title: Class 'dgirtIn': a minimal example object
### Aliases: toy_dgirt_in

### ** Examples

toy_dgirt_in



cleanEx()
nameEx("toy_dgirtfit")
### * toy_dgirtfit

flush(stderr()); flush(stdout())

### Name: toy_dgirtfit
### Title: Class 'dgirtfit': a minimal example object
### Aliases: toy_dgirtfit

### ** Examples

toy_dgirtfit



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
