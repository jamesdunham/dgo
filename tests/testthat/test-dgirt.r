context("Write data with stan_rdump")
suppressMessages({
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

  context("Read optimize output")

  # create output
  save_pars = c("theta_bar", "xi", "gamma", "delta_gamma", "delta_tbar", "nu_geo",
    "nu_geo_prior", "kappa", "sd_item", "sd_theta", "sd_theta_bar", "sd_gamma", "sd_innov_gamma",
    "sd_innov_delta", "sd_innov_logsd", "sd_total", "theta_l2", "var_theta_bar_l2")
  d = expand.grid(save_pars, 1:2, 1:2) %>%
    dplyr::mutate_("parname" = ~paste(Var1, Var2, Var3, sep = '.'),
                   "value" = ~rnorm(n = length(parname))) %>%
    dplyr::select_(.dots = c("parname", "value")) %>%
    reshape2::dcast(. ~ parname) %>% dplyr::select_(~-one_of("."))
  write.csv(d, "test_output.csv", row.names = FALSE)

  expect_silent(suppressMessages(read_cmdstan_output("test_output.csv", save_pars)))
  output = read_cmdstan_output("test_output.csv", save_pars)
  expect_true(identical(colnames(output), c("param", "value")))
  expect_true(identical(dim(output), c(length(save_pars) * 2L * 2L, 2L)))
  expect_true(inherits(output$param, "character"))
  expect_true(inherits(output$value, "numeric"))

  context("Filter optimize output")

  param_filter = c("theta_bar", "xi")
  expect_silent(filter_cmdstan_output(output, param_filter))
  filtered_output = filter_cmdstan_output(output, param_filter)
  expect_equal(length(filtered_output), length(param_filter))
  expect_equal(names(filtered_output), param_filter)
  expect_equal(dim(filtered_output[[param_filter[1]]]), c(4L, 2L))
  expect_equal(dim(filtered_output[[param_filter[2]]]), c(4L, 2L))
  expect_error(filter_cmdstan_output(output, "foo"), "output does not contain estimates")
})
