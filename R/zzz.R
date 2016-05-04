.onLoad <- function(libname, pkgname) {
  if (!substr(as.character(sys.call(1L))[1], 1, 8) == "devtools") {
    loadRcppModules()
  } else if (!is.loaded("stan_fit42016_04_20_mod")) {
    loadModule("stan_fit42016_04_20_mod", TRUE)
    # res <- dyn.load("~/projects/dgirt/src/dgirt.so")
    # mod <- Rcpp::Module("stan_fit42016_04_20_mod", PACKAGE = res)
    # source('R/stanmodels.R')
    # stanmodels <- new(stanmodels)
  }
}
