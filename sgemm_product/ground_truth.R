library(parallel)
library(microbenchmark)
library(fastglm)

setwd("sgemm_product")
sgemm <- cbind(1, read.csv("sgemm_product.csv"))
sgemm$run_mean <- rowMeans(sgemm[,16:19])
sgemm <- as.matrix(sgemm)

source("../functions.R")
source("../runner_functions.R")
source("../sgd_experiment/likelihoods.R")
model_partitions <- align_models(1:(2^14))
n_obs <- 241600
name <- "full_sgemm3"
directory <- "truth"

run_multisim(sgemm[,(1:15)], sgemm[,20], linear.g.prior.loglik, model_partitions, n_obs, name, directory, params=list(g=14^2))

load("truth/full_sgemm.Rdata")

full_sgemm_renorm2 <- GMJMCMC:::marginal.probs.renorm(full_sgemm)

library(stochQN)
library(fastglm)
library(irls.sgd)

microbenchmark(irlsres <- irls.sgd(mill_x_g, mill_y_l1M, binomial(), irls.control = list(subs = 0.0001, maxit = 50, tol = 1e-07, cooling = c(3, 0.9, 0.95), expl = c(3, 1.5, 1)), sgd.control = list(subs = 0.0001, alpha = 0.001, maxit=300), save_hist = F), times=5)


load("../sgd_experiment/data/data.RData")
y_10K <- as.numeric(y_10K)

microbenchmark(glmm <- fastglm(mill_x_g, mill_y_l1M, family=binomial()), times=1)

cbind(glmm$coefficients, model$coef)

irls.sgd::get_deviance(model$coef, x_10K, y_10K, family=binomial())
irls.sgd::get_deviance(glmm$coefficients, mill_x_g, mill_y_l1M, family=binomial())
irls.sgd::get_deviance(irlsres$coefficients, mill_x_g, mill_y_l1M, family=binomial())
irls.sgd::get_deviance(rnorm(16), x_10K, y_10K, family=binomial())

source("stochQN.R")

mill_y_l1M <- as.numeric(mill_y_l1M)

model <- stochastic.logistic.regression(dim=15, lambda=0, optimizer="SQN")

model$stochQN$grad_fun <- logistic_grad_cpp
model$stochQN$hess_vec_fun <- logistic_hess_vec_cpp
model$stochQN$pred_fun <- logistic_pred_cpp

library(profvis)
library(microbenchmark)

microbenchmark(devs <- sqn_run(mill_x_l1M, mill_y_l1M, model, 100, 200), times=1)



plot(res1, type="l")
plot(dev_list6, type="l")
lines(dev_list3, col="blue")
lines(dev_list4, col="green")

library(Rcpp)

setwd("thesis_supplementary")
source("logis_sim_data.R")

sourceCpp("logistic.cpp")

sqn_run <- function (x, y, model, batch_size, iter) {
  for (i in seq_len(iter)) {
    batch <- sample(1000000,
      size = batch_size, replace=TRUE)
    partial_fit_logistic(model, x[batch ,-1], y[batch])

  }
  deviance <- irls.sgd::get_deviance(model$coef, x, y, family=binomial())
  return(deviance)
}


