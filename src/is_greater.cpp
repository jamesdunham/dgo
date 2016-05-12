#include <Rcpp.h>

using namespace Rcpp;

//' @export
// [[Rcpp::export()]]
DataFrame is_greater (NumericVector response, IntegerVector levels) {

  int J = levels.size();
  int N = response.size();
  List res(J);
  StringVector col_names(J);

  for (int j = 0 ; j < J ; j++ ) {
    IntegerVector vec(N);
    for (int i = 0 ; i < N ; i++) {
      if (response(i) > levels[j]) {
        vec(i) = 1;
      } else if (response(i) <= levels[j]) {
        vec(i) = 0;
      } else {
        vec(i) = NA_INTEGER; 
      }
    }
    res[j] = vec;
    char name[6];
    sprintf(&(name[0]), "gt_%d", j + 1);
    col_names(j) = name;
  }
  
  res.attr("names") = col_names;

  return(res);
}
