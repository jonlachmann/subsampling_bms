#include <Rcpp.h>
#include <RcppEigen.h>

//[[Rcpp::depends(RcppEigen)]]
//' Logistic gradient
//'
//' @param data A matrix with the data to use
//' @param theta The coefficient vector
//' @param idx The rows to use (base 0 for now)
//' @export
//[[Rcpp::export]]
Eigen::VectorXd logistic_grad_cpp(const Eigen::Map<Eigen::VectorXd> &theta,
                          const Eigen::Map<Eigen::MatrixXd> &x,
                          const Eigen::Map<Eigen::VectorXd> &y,
                          Rcpp::Nullable<Rcpp::NumericVector> weights) {
    int n = x.rows();
    Eigen::VectorXd eta = 1/(1+exp(-(x*theta).array()));
    return x.transpose()*(eta-y)/n;
}

//[[Rcpp::depends(RcppEigen)]]
//' Logistic vector Hessian product
//'
//' @param data A matrix with the data to use
//' @param theta The coefficient vector
//' @param idx The rows to use (base 0 for now)
//' @export
//[[Rcpp::export]]
Eigen::VectorXd logistic_hess_vec_cpp(const Eigen::Map<Eigen::VectorXd> &theta,
                          const Eigen::Map<Eigen::VectorXd> &vec,
                          const Eigen::Map<Eigen::MatrixXd> &x,
                          const Eigen::Map<Eigen::VectorXd> &y,
                          Rcpp::Nullable<Rcpp::NumericVector> weights) {
    Eigen::VectorXd eta = 1 / (1 + exp(-(x * theta).array()));
    Eigen::VectorXd diag = eta.array() * (1 - eta.array());
    Eigen::VectorXd Hp = (x * diag.asDiagonal()).transpose() * (x * vec);
    Hp = Hp / x.rows();
    return Hp;
}

//[[Rcpp::depends(RcppEigen)]]
//' Logistic loss
//'
//' @param data A matrix with the data to use
//' @param theta The coefficient vector
//' @param idx The rows to use (base 0 for now)
//' @export
//[[Rcpp::export]]
double logistic_loss_cpp(const Eigen::Map<Eigen::VectorXd> &theta,
                          const Eigen::Map<Eigen::MatrixXd> &x,
                          const Eigen::Map<Eigen::VectorXd> &y,
                          Rcpp::Nullable<Rcpp::NumericVector> weights) {
	Eigen::VectorXd eta = 1 / (1 + exp(-(x * theta).array()));
	double logloss = (-(y.array() * log(eta.array()) + (1 - y.array()) * log(1 - eta.array()))).mean();
	return logloss;
}

//[[Rcpp::depends(RcppEigen)]]
//' Logistic prediction
//'
//' @param data A matrix with the data to use
//' @param theta The coefficient vector
//' @param idx The rows to use (base 0 for now)
//' @export
//[[Rcpp::export]]
Eigen::VectorXd logistic_pred_cpp(const Eigen::Map<Eigen::VectorXd> &theta,
                          const Eigen::Map<Eigen::MatrixXd> &x,
                          const Eigen::Map<Eigen::VectorXd> &y,
                          Rcpp::Nullable<Rcpp::NumericVector> weights) {
	return 1 / (1 + exp(-(x * theta).array()));
}
