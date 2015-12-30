context("Write data with stan_rdump")
d <- list(a = 1L)

dump_dgirt(d)
expect_true(file.exists(system.file("dgirt_data.Rdump", package = "dgirt")))
unlink(system.file("dgirt_data.Rdump", package = "dgirt"))

expect_silent(dump_dgirt(d))
expect_error(dump_dgirt(NULL))
expect_error(dump_dgirt(data.frame()))

context("Get dgirt executable path")

expect_match(get_dgirt_path(), "dgirt$")
expect_true(stringi::stri_length(get_dgirt_path()) > 0)

context("Get cmdstan output path")

expect_match(get_output_path(), "/output.csv$")
expect_true(stringi::stri_length(get_output_path()) >
  stringi::stri_length("/output.csv"))

context("Get stan_rdump output path")

expect_match(get_dump_path(), "/dgirt_data.Rdump$")
expect_true(stringi::stri_length(get_dump_path()) >
  stringi::stri_length("/dgirt_data.Rdump"))

context("Run cmdstan")

expect_error(read_cmdstan_output("\n"))

