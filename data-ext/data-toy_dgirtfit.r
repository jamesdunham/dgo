library(dgo)
data(opinion)
data(toy_dgirt_in)
toy_dgirtfit <- dgirt(toy_dgirt_in, iter = 100, chains = 2, cores = 2)
devtools::use_data(toy_dgirtfit, overwrite = TRUE)
tools::checkRdaFiles(system.file('data', package = 'dgo'))
tools::resaveRdaFiles(system.file('data', package = 'dgo'))
#