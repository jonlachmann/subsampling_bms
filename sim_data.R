nvars <- 15
nobs <- 10^6
full_model_count <- 2^nvars

if (exists("generate")) {
  print("Generating data...")
  library(mvtnorm)
  set.seed(1911)
  #save(covmat, file="data/covmat.Rdata")
  #covmat <- cov(read.table(text=getURL("https://raw.githubusercontent.com/aliaksah/EMJMCMC2016/master/examples/Simulated%20Data%20%28Example%201%29/simcen-x.txt")))
  load(file="data/covmat.Rdata")

  mill_x_g <- matrix(rmvnorm(nobs, rnorm(nvars), covmat), nobs)

  betas_g <- c(0.48, 8.72, 1.76, 1.87, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0)
  mill_x_g <- cbind(1,mill_x_g)

  ic <- 2
  # Decrease effects with sqrt of sample size increase factor
  mill_y_g1M <- rnorm(nobs, ((mill_x_g[,2:16] %*% (betas_g/sqrt(10000))) + ic), 1)
  mill_y_g100K <- rnorm(nobs, ((mill_x_g[,2:16] %*% (betas_g/sqrt(1000))) + ic), 1)
  mill_y_g10K <- rnorm(nobs, ((mill_x_g[,2:16] %*% (betas_g/sqrt(100))) + ic), 1)
  mill_y_g4K <- rnorm(nobs, ((mill_x_g[,2:16] %*% (betas_g/sqrt(40))) + ic), 1)
  mill_y_g2K <- rnorm(nobs, ((mill_x_g[,2:16] %*% (betas_g/sqrt(20))) + ic), 1)

  if (exists("save_data")) {
    print("Saving data...")
    mill_x_g_1 <- mill_x_g[1:500000,]
    mill_x_g_2 <- mill_x_g[500001:1000000,]
    save(mill_x_g_1, file="data/mill_x_g_1.Rdata")
    save(mill_x_g_2, file="data/mill_x_g_2.Rdata")
    save(mill_y_g1M, file="data/mill_y_g1M.Rdata")
    save(mill_y_g100K, file="data/mill_y_g100K.Rdata")
    save(mill_y_g10K, file="data/mill_y_g10K.Rdata")
  }
} else {
# If we are not generating data, just read it from disk
  print("Loading data...")
  ex1_sim_files <- list.files(path="data/", pattern=".Rdata")
  for (file in ex1_sim_files) load(file=paste0("data/",file))
  mill_x_g <- rbind(mill_x_g_1, mill_x_g_2)
  remove(mill_x_g_1, mill_x_g_2, ex1_sim_files)
}

# Generate if that flag is set, otherwise all data is loaded when sourcing the gauss_sim_data.R file
if (exists("generate")) {
  print("Generating data...")
  set.seed(1911)
  mill_y_eta_l1M <- (1/(1+exp(-mill_y_g1M+mean(mill_y_g1M))))
  mill_y_l1M <- rbinom(nobs, 1, mill_y_eta_l1M)

  mill_y_eta_l100K <- (1/(1+exp(-mill_y_g100K+mean(mill_y_g100K))))
  mill_y_l100K <- rbinom(nobs, 1, mill_y_eta_l100K)

  mill_y_eta_l10K <- (1/(1+exp(-mill_y_g10K+mean(mill_y_g10K))))
  mill_y_l10K <- rbinom(nobs, 1, mill_y_eta_l10K)

  mill_y_eta_l4K <- (1/(1+exp(-mill_y_g4K+mean(mill_y_g4K))))
  mill_y_l4K <- rbinom(nobs, 1, mill_y_eta_l4K)

  mill_y_eta_l2K <- (1/(1+exp(-mill_y_g2K+mean(mill_y_g2K))))
  mill_y_l2K <- rbinom(nobs, 1, mill_y_eta_l2K)
  if (exists("save_data")) {
    print("Saving data...")
    save(mill_y_l1M, file="data/mill_y_l1M.Rdata")
    save(mill_y_l100K, file="data/mill_y_l100K.Rdata")
    save(mill_y_l10K, file="data/mill_y_l10K.Rdata")
  }
}