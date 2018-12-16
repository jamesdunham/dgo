# NOTE: If the mgirt branch is installed, load it like this
library(dgo)
# Otherwise, point load_all to the path where the mgirt branch is downloaded,
# like
devtools::load_all('~/dev/dgo')
stopifnot(packageVersion('dgo') >= '0.3.0')

require(haven)
require(tidyr)

d = haven::read_dta('~/Downloads/hostile_sexism.dta')

# Don't recompile the model if we've already compiled and saved it in a previous
# session
if (file.exists('model.Rds')) {
  model = readRDS('model.Rds')
} else {
  model = NULL
}

# We'll get a warning about attributes when melting to long; this refers to
# Stata column metadata read by haven and associated with columns as attributes,
# and can be ignored
shaped_data = shape_multinomial(d,
  item_names = paste0('sexism', 1:4),
  time_name = NULL,
  geo_name = 'inputstate',
  group_names = c('pid3', 'race2'),
  weight_name = 'weight')

# Fit the model
fit = mgirt(shaped_data, iter = 10, model = model, chains = 4, cores = 4)

# Save the compiled model for use in future sessions 
saveRDS(fit@stanmodel, 'model.Rds')

# NOTE: the dgo post-estimation functions and class methods aren't yet adapted
# for mgirt; the rest of this is manual 

# Extract group means
estimates <- as.data.frame.matrix(t(as.matrix(fit, pars = 'bar_theta')))

# Extract the geo x group names from the shaped data
group_names = dimnames(shaped_data$SSSS)[[2]]
# NOTE: there's one time period and one dimension; in this special case, each
# row represents a group and we can simply assign the group names to a new
# column; in the general case, we have to map the parameter indices to the
# corresponding T x G x D labels 
estimates$group = group_names

# The group name is formatted 'inputstate | pid3 | race2'
head(estimates$group)

# Expand the group identifiers
group_names = c('inputstate', 'pid3', 'race2')
estimates = tidyr::separate(estimates, 'group', group_names, sep = '\\|')

# The V1 ... V20 variables represent iterations; melt them
estimates = melt(estimates, id.vars = group_names, variable = 'iter')
# The variable column is a factor, so as.integer gives us V1 => 1, etc.
estimates$iter = as.integer(estimates$iter)

# Ready for summary
head(estimates)
