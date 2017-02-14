# This approach to exposing module content avoids the .onLoad() and
# loadRcppModules() functions, and needing to keep an updated list of modules in
# the DESCRIPTION file. Requires Rcpp >= 0.9.11.
# See https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-modules.pdf.
Rcpp::loadModule("stan_fit42017_01_04_singleissue_mod", TRUE)
Rcpp::loadModule("stan_fit42017_01_04_mod", TRUE)

