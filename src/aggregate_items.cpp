#include <Rcpp.h>

using namespace Rcpp;

//' @export
// [[Rcpp::export()]]
DataFrame agg_items (DataFrame x, NumericVector row_n, NumericVector wt, IntegerVector grp) {
  CharacterVector nm = x.attr("names");
  IntegerVector groups = sort_unique(grp);
  int N = nm.size() * groups.size();
  IntegerVector group_res(N);
  CharacterVector name_res(N);
  NumericVector item_n(N);
  NumericVector item_s(N);
  int i = 0;
    for (int k = 0; k < x.size(); k++) {
      NumericVector x_k = x(k);
      for (int g = 0; g < groups.size(); g++) {
      LogicalVector obs = !is_na(x_k) & grp == groups[g];
      NumericVector x_obs = x_k[obs];
      NumericVector row_n_obs = row_n[obs];
      NumericVector wt_obs = wt[obs];
      if (x_obs.size() > 0) {
        double def = 1 + pow(sd(wt_obs) / mean(wt_obs), 2);
        item_n(i) = ceil(sum(1 / row_n_obs / def));
        NumericVector adj_wt = wt_obs / item_n(k);
        double y_star_bar = sum(x_obs * adj_wt / sum(adj_wt));
        item_s(i) = round(y_star_bar * item_n(k));
      } else {
        item_s(i) = 0;
        item_n(i) = 0;
      }
      group_res(i) = groups[g];
      name_res(i) = nm[k];
      i++;
    }
  }
  return(DataFrame::create(_["item"] = name_res,
                           _["group"] = group_res,
                           _["N_grp"] = item_n,
                           _["S_grp"] = item_s));
}
