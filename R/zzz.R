# Reload modules on devtools::load_all()
if (substr(as.character(sys.call(1L))[1], 1, 8) == "devtools") {
  .onAttach <- function(libname, pkgname) {
    Rcpp::loadModule("stan_fit42016_04_20_mod", TRUE)
  }
}
