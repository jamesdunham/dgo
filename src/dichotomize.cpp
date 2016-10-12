#include <Rcpp.h>
#include <vector>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export()]]
DataFrame dichotomize_cpp (NumericVector response) {
  // Create a dataframe whose columns indicate whether input values' ranks are
  // greater than a given rank.
  //
  // Given k unique input values, this function returns k-1 columns.

  NumericVector uniques = sort_unique(response);
  vector<int> ranks(uniques.size());  // The values associated with each rank

  // Filter out non-integers like NA_INTEGER
  int k = 0;
  for (int i = 0; i < uniques.size(); ++i) {
     if (uniques[i] == uniques[i]) {
       ranks[i] = uniques[i];
       k++;
     }
  }
  ranks.resize(k);

  // k ranks will result in k-1 columns
  int ncols;
  if (ranks.size() < 2) {
    ncols = 1;
  } else {
    ncols = ranks.size() - 1;
  }

  List ret(ncols);
  StringVector col_names(ncols);

  // Build up the indicator columns
  for (int j = 0 ; j < ncols ; j++) {
    IntegerVector is_greater(response.size());
    for (int i = 0 ; i < response.size() ; i++) {
      if (response(i) > ranks[j]) {
        is_greater(i) = 1;
      } else if (response(i) <= ranks[j]) {
        is_greater(i) = 0;
      } else {
        is_greater(i) = NA_INTEGER;
      }
    }

    // Columns will be named _gt1, _gt2, ...
    char name[6];
    sprintf(&(name[0]), "_gt%d", j + 1);
    col_names(j) = name;

    ret[j] = is_greater;
  }

  ret.attr("names") = col_names;
  return(wrap(ret));
}
