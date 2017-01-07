library(dgo)
data(opinion)
toy_dgirt_in <- shape(opinion,
                 time_name = "year",
                 item_names = "gaymarriage_amendment",
                 geo_name = "state",
                 group_names = "race",
                 survey_name = "source",
                 geo_filter = c("VA", "SC"),
                 weight_name = "weight")
devtools::use_data(toy_dgirt_in, overwrite = TRUE)
tools::checkRdaFiles(system.file('data', package = 'dgo'))
tools::resaveRdaFiles(system.file('data', package = 'dgo'))
