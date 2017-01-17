library(data.table)

data(opinion)
data(targets)
setDT(targets)

head(opinion)
head(targets)

opinion[, .N, by = "race3"]
targets[, .N, by = "race3"]
setnames(opinion, "race", "race3")

opinion[, .N, by = "education"]
targets[, .N, by = "education"]

opinion[, education := as.character(education)]
opinion[education == "1", education := "no hs"]
opinion[education == "2", education := "hs degree"]
opinion[education == "3", education := "some college"]
opinion[education == "4", education := "college degree"]
opinion[education == "5", education := "college degree"]

devtools::use_data(opinion, overwrite = TRUE)

