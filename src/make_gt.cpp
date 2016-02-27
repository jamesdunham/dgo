# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp ;

// [[Rcpp::export()]]
arma::vec make_gt (arma::vec values,
                   int gt
                   ) {

  int N = values.size();
  arma::vec res = arma::vec(N);

  for (int i = 0 ; i < N ; i++) {
    if (values(i) > gt) {
      res(i) = 1;
    } else {
      res(i) = 0;
    }
  }

  return(res);
}
