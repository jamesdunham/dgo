# https://github.com/jimhester/lintr#testthat
if (requireNamespace("lintr", quietly = TRUE)) {
  testthat::context("lints")
  testthat::test_that("Package Style", {
    devtools::lint(".", linters = lintr::with_defaults(line_length_linter = lintr::line_length_linter(110)))
  })
}
