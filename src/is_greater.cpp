#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export()]]
Rcpp::IntegerVector is_greater (Rcpp::NumericVector response, int level) {

  int N = response.size();
  Rcpp::IntegerVector res = Rcpp::IntegerVector(N);

  for (int i = 0 ; i < N ; i++) {
    if (response(i) > level) {
      res(i) = 1;
    } else if (response(i) <= level) {
      res(i) = 0;
    } else {
      res(i) = NA_INTEGER; 
    }
  }

  return(res);
}
