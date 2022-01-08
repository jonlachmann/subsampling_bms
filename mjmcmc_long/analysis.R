source("mjmcmc_long/rmse_funs.R")

load("mjmcmc_long/8000_result.RData")
load(file="mjmcmc_long/full_10Kg.Rdata")

full_10Kg_renorm <- GMJMCMC:::marginal.probs.renorm(full_10Kg)

rmseconv <- rmse_conv(full_10Kg_renorm, result, 80,F)

plot(rmseconv, type="l")