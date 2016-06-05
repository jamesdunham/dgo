test_that("dgirt_plot and plot can be called", {
  expect_silent(plot(toy_dgirtfit))
  expect_silent(dgirt_plot(toy_dgirtfit))
})


test_that("plots can be amended", {
  expect_silent(dgirt_plot(toy_dgirtfit) %+% ylab("foo"))
})

test_that("plot calls dgirt_plot", {
  p <- plot(toy_dgirtfit)
  dp <- dgirt_plot(toy_dgirtfit)
  expect_equal(p, dp)
})
