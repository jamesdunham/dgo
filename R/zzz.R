.onLoad <- function(libname, pkgname) {
  if (!substr(as.character(sys.call(1L))[1], 1, 8) == "devtools") {
    cat("loadRcppModules!")
    Rcpp::loadRcppModules()
    # } else if (!".__C__Rcpp_model_2016_04_20" %in% ls(all.names = TRUE)) {
    #   Rcpp::loadModule("stan_fit42016_04_20_mod", TRUE)
    #   # res <- dyn.load("~/projects/dgirt/src/dgirt.so")
    #   # mod <- Rcpp::Module("stan_fit42016_04_20_mod", PACKAGE = res)
    #   # source('R/stanmodels.R')
    #   # stanmodels <- new(stanmodels)
    # }
  }
}

.onAttach <- function(libname, pkgname) {
    Rcpp::loadModule("stan_fit42016_04_20_mod", TRUE)
}
