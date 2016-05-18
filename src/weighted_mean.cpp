#include <Rcpp.h>

using namespace Rcpp;

//' @export
// [[Rcpp::export()]]
double weighted_mean (NumericVector x, NumericVector wt) {
  LogicalVector obs = !is_na(x);
  NumericVector x_obs = x[obs];
  NumericVector wt_obs = wt[obs];
  if (x_obs.size() > 0) {
    return(sum(x_obs * wt_obs / sum(wt_obs)));
  } else {
    return(NA_REAL);
  }
}
