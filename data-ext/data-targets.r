original <- readRDS('~/Downloads/targets_dgo.rds')
summary(original)
original[sapply(original, is.factor)] <- lapply(original[sapply(original, is.factor)], 
  as.character)
targets <- original
devtools::use_data(targets, overwrite = TRUE)
