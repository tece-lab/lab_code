
#include <Rcpp.h>

// [[Rcpp::export]]
double calc_square(double input) {
    double output = input * input;
    return output;
}

// [[Rcpp::export]]
Rcpp::NumericVector calc_squares(Rcpp::NumericVector v) {
    Rcpp::NumericVector output(v.size());
    for(int i = 0; i < v.size(); ++i) {
        output[i] = v[i] * v[i];
    }
    return(output);
}

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix calc_squares_matrix(NumericMatrix m) {
    NumericMatrix output(m.nrow(), m.ncol());
    for(int i = 0; i < m.nrow(); ++i) {
        output(i, _) = calc_squares( m(i, _) );
    }
    return(output);
}

// [[Rcpp::export]]
// example from: http://gallery.rcpp.org/articles/random-number-generation/
NumericMatrix rngCpp(const int N) {
    NumericMatrix X(N, 4);
    X(_, 0) = runif(N); // N values in [0,1]
    X(_, 1) = rnorm(N); // N values with mu = 0, sd = 1
    X(_, 2) = rgamma(N, 5); // N values with shape = 5
    X(_, 3) = rpois(N, 10); //N values with lambda = 10
    return X;
}
