#include <Rcpp.h>

using namespace Rcpp;

//' @export
// [[Rcpp::export()]]
List count_items_cpp (NumericVector x, NumericVector row_n, NumericVector wt) {
  LogicalVector obs = !is_na(x);
  NumericVector x_obs = x[obs];
  NumericVector row_n_obs = row_n[obs];
  if (x_obs.size() > 0) {
    NumericVector wt_obs = wt[obs];
    double def = 1 + pow(sd(wt_obs) / mean(wt_obs), 2);
    double n = ceil(sum(1 / row_n_obs / def));
    NumericVector adj_wt = wt_obs / n;
    double y_star_bar = sum(x_obs * adj_wt / sum(adj_wt));
    double s = round(y_star_bar * n);
    return(List::create(_["N_grp"] = n, _["S_grp"] = s));
  } else {
    return(List::create(_["N_grp"] = NA_REAL, _["S_grp"] = NA_REAL));
  }
}
