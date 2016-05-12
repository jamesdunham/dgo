library(dgirt)
data(opinion)
toy_dgirt_in <- shape(opinion,
                 time_name = "year",
                 item_names = "Q_cces2006_gaymarriageamendment",
                 geo_name = "state",
                 group_names = "race",
                 survey_name = "source",
                 geo_filter = c("VA", "SC"),
                 weight_name = "weight")
devtools::use_data(toy_dgirt_in, overwrite = TRUE)
tools::checkRdaFiles(system.file('data', package = 'dgirt'))
tools::resaveRdaFiles(system.file('data', package = 'dgirt'))
