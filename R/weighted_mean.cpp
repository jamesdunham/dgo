// [[Rcpp::depends(RcppNT2)]]
#include <RcppNT2.h>
#include <Rcpp.h>
using namespace RcppNT2;
using namespace Rcpp;

struct simd_plus {
   template <typename T>
   T operator()(const T& lhs, const T& rhs) {
      return lhs + rhs;
   }
};

template <typename V>
class DotProductMapReducer
{
public:

  // The initial value for our accumulation. In this case, for
  // a 'plus' reduction, we start at 0.
  V init() { return V{}; }

  // 'map' describes the transformation -- here, multiplying
  // two elements together.
  template <typename T>
  T map(const T& lhs, const T& rhs)
  {
    return lhs * rhs;
  }

  // 'combine' describes how 'map'ped values should be combined.
  // Note that the return type matches the input type -- this
  // allows this function to be specialized both for the pack
  // data structure, as well as scalars, when appropriate.
  template <typename T>
  T combine(const T& lhs, const T& rhs)
  {
    return lhs + rhs;
  }

  // 'reduce' is our bridge from SIMD land to scalar land.
  // We collapse our SIMD vector into a scalar value by
  // summing the elements.
  template <typename T>
  auto reduce(const T& t) -> decltype(nt2::sum(t))
  {
    return nt2::sum(t);
  }
};

// [[Rcpp::export]]
double simdDot(NumericVector x, NumericVector y)
{
  return variadic::simdMapReduce(DotProductMapReducer<double>(), x, y);
}

// [[Rcpp::export]]
double simdSum(NumericVector x)
{
   return simdReduce(x.begin(), x.end(), 0.0, simd_plus());
}

// [[Rcpp::export]]
double simdWeightMean(NumericVector x, NumericVector w)
{
  return simdDot(x, w) / simdSum(w);
} 

struct divide
{
   template <class T>
   T operator()(const T& lhs, const T& rhs) const
   {
      return lhs / rhs;
   }
};

// [[Rcpp::export]]
NumericVector simd_divide(NumericVector lhs, NumericVector rhs)
{
   NumericVector result = no_init(lhs.size());
   simdTransform(lhs.begin(), lhs.end(), rhs.begin(), result.begin(), divide());
   return result;
}
