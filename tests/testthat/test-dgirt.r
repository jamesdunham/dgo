context("Write data with stan_rdump")
d = list("a" = 1L)

dump_dgirt(d)
expect_true(file.exists(system.file("inst/dgirt_data.Rdump", 
 package = "dgirt")))
unlink(system.file("inst/dgirt_data.Rdump", package = "dgirt"))

expect_silent(dump_dgirt(d))
expect_error(dump_dgirt(NULL))
expect_error(dump_dgirt(data.frame()))

context("Get dgirt executable path")

expect_match(get_dgirt_path(), "dgirt$")
expect_true(stringr::str_length(get_dgirt_path()) > 0)

context("Get cmdstan output path")

expect_match(get_output_path(), "/output.csv$")
expect_true(stringr::str_length(get_output_path()) >
  stringr::str_length("/output.csv"))

context("Get stan_rdump output path")

expect_match(get_dump_path(), "/dgirt_data.Rdump$")
expect_true(stringr::str_length(get_dump_path()) >
  stringr::str_length("/dgirt_data.Rdump"))

