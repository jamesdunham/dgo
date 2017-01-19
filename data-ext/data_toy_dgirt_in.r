library(dgo)
data(opinion)
toy_dgirt_in <- shape(opinion,
                 time_name = "year",
                 item_names = c("affirmative_action", "gaymarriage_amendment"), 
                 geo_name = "state",
                 group_names = "race3",
                 survey_name = "source",
                 geo_filter = c("VA", "SC"),
                 weight_name = "weight")
devtools::use_data(toy_dgirt_in, overwrite = TRUE)
