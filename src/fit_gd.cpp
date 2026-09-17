#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List gd_cpp(NumericVector x, NumericVector y, double alpha, int iterations) {
  int n = x.length();
  double slope = 0.0;
  double intercept = 0.0;

  for(int i = 0; i < iterations; i++) {
    double sum_slope = 0.0;
    double sum_intercept = 0.0;

    for(int j = 0; j < n; j++) {
      double y_pred = slope * x[j] + intercept;
      sum_slope += -2.0 * x[j] * (y[j] - y_pred);
      sum_intercept += -2.0 * (y[j] - y_pred);
    }

    slope = slope - alpha * (sum_slope / n);
    intercept = intercept - alpha * (sum_intercept / n);
  }

  return List::create(Named("slope") = slope,
                      Named("intercept") = intercept);
}
