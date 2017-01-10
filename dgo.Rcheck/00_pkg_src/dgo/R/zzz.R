.onLoad <- function(libname, pkgname) {
  assertthat::assert_that(assertthat::not_empty(stanmodels))
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) Rcpp::loadModule(m, what = TRUE)
}
