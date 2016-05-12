#include <Rcpp.h>

using namespace Rcpp;

//' @export
// [[Rcpp::export()]]
DataFrame is_greater (NumericVector response) {

  NumericVector rlevels = sort_unique(response);

  std::vector<int> gt(rlevels.size());
  int k = 0;
  for (int i = 0; i < rlevels.size(); ++i) {
     if (rlevels[i] == rlevels[i]) {
       gt[i] = rlevels[i];
       k++;
     }
  }
  gt.resize(k);

  int J;
  if (gt.size() < 2) {
    J = 1;
  } else {
    J = gt.size() - 1;
  }
  List res(J);
  StringVector col_names(J);
  int N = response.size();

  for (int j = 0 ; j < J ; j++ ) {
    IntegerVector vec(N);
    for (int i = 0 ; i < N ; i++) {
      if (response(i) > gt[j]) {
        vec(i) = 1;
      } else if (response(i) <= gt[j]) {
        vec(i) = 0;
      } else {
        vec(i) = NA_INTEGER; 
      }
    }
    res[j] = vec;
    char name[6];
    sprintf(&(name[0]), "_gt%d", j + 1);
    col_names(j) = name;
  }

  res.attr("names") = col_names;

  return(res);
}
