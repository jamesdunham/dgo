devtools::load_all()
data(toy_dgirt_in)
data(toy_dgirtfit)
data(opinion)

dgirt_ret <- dgirt(toy_dgirt_in, iter = 5, chains = 1, seed = 42,
  model = toy_dgirtfit@stanmodel)
saveRDS(dgirt_ret, "tests/testthat/dgirt_ret.Rds")

dgmrp_in <- shape(opinion, item_names = "abortion", time_name = "year",
  geo_name = "state", group_names = "race3", geo_filter = c("CA", "GA"))

dgmrp_ret <- dgmrp(dgmrp_in, iter = 5, chains = 1, seed = 42,
  model = toy_dgirtfit@stanmodel)
saveRDS(dgmrp_ret, "tests/testthat/dgmrp_ret.Rds")
