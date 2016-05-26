#include <Rcpp.h>

using namespace Rcpp;

//' @export
// [[Rcpp::export()]]
DataFrame agg_items (DataFrame x, NumericVector row_n, NumericVector wt) {

  NumericVector n(x.length());
  NumericVector s(x.length());
  
  double deff;
  if (x.nrows() > 1) {
    deff = 1 + pow(sd(wt) / mean(wt), 2);
  } else {
    deff = 1;
  }
  // std::cout << "deff: " << deff << std::endl << std::endl;
  // std::cout << "wt: " << sd(wt) << std::endl << std::endl;

  for (int k = 0; k < x.size(); k++) {
    NumericVector col = x(k);
    LogicalVector is_obs = !is_na(col);
    NumericVector col_obs = col[is_obs];
    NumericVector row_n_obs = row_n[is_obs];
    NumericVector wt_obs = wt[is_obs];
    if (col_obs.size() > 0 & row_n_obs.size() > 0) {

      // std::cout << "rown for " << nm(k) << std::endl;
      n(k) = ceil(sum(1 / row_n_obs / deff));

      NumericVector adj_wt = wt_obs / row_n_obs;
      // adj_wt[is_na(adj_wt)] = 0;

      // std::cout << "weights for " << nm(k) << std::endl;
      // std::cout << adj_wt << std::endl << std::endl;

      // group mean 
      double ystar = sum(col_obs * adj_wt) / sum(adj_wt);

      // success count is group mean * trial count
      s(k) = round(ystar * n(k));

    } else {
      s(k) = 0;
      n(k) = 0;
    }
  }

  return(DataFrame::create(_["item"] = x.attr("names"),
                           _["n_grp"] = n,
                           _["s_grp"] = s));
}
