# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-12-09

logistic.loglik.bic.sgd <- function (y, x, model, maxit) {
  mod <- irls.sgd(as.matrix(x[,model]), y, binomial(),
            irls.control=list(maxit=0),
            sgd.control=list(subs=0.001, maxit=maxit, alpha=0.2, decay=0.99995, histfreq=10))
  ret <- mod$deviance
  return(ret)
}

logistic.loglik.bic.irlssgd <- function (y, x, model) {
  mod <- irls.sgd(as.matrix(x[,model]), y, binomial(),
            irls.control=list(subs=0.001, maxit=75, tol=1e-7, cooling = c(3,0.9,0.95), expl = c(3,1.5,1)),
            sgd.control=list(subs=0.001, maxit=500, alpha=0.05, decay=0.99, histfreq=10))
  ret <- mod$deviance
  return(ret)
}

logistic.loglik.bic <- function (y, x, model) {
  mod <- glm.fit(as.matrix(x[,model]), y, family=binomial())
  ret <- mod$deviance
  return(ret)
}

linear.g.prior.loglik.irlssgd <- function (y, x, model, complex, params) {
  mod <- irls.sgd(as.matrix(x[,model]), y, gaussian(),
            irls.control=list(subs=params$subs, maxit=20, tol=1e-7, cooling = c(1,0.9,0.75), expl = c(3,1.5,1)),
            sgd.control=list(subs=params$subs, maxit=250, alpha=0.001, decay=0.99, histfreq=10))
  rsquared <- 1-sum(var(y-x[,model,drop=F]%*%mod$coefficients))/sum(var(y))
  p <- mod$rank
  n <- nrow(x)
  logmarglik <- 0.5*(log(1+params$g)*(n-p) - log(1+params$g*(1-rsquared))*(n-1))*(p!=1)
  return(logmarglik)
}