library(GMJMCMC)
library(irls.sgd)

source("../sgd_experiment/likelihoods.R")

load(file="data_x.RData")
load(file="data_y_10K.RData")
load(file="data_y_100K.RData")

n_obs <- 100000
n_iter <- 1000000

data_use <- cbind(data_y_100K, data_x[1:n_obs,])

sim_probs <- gen.probs.list()
sim_pars <- gen.params.list(data_use)
sim_pars$loglik$g <- 100/sqrt(n_obs/100)
sim_pars$loglik$subs <- 0.0075
sim_probs$large <- 0.05

mjmcmc_res <- mjmcmc(data_use, linear.g.prior.loglik.irlssgd, n_iter, sim_probs, sim_pars, TRUE)