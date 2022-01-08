# Calculate rmse when also having the full renormalized probabilities
rmse <- function (full, models, renorm=T) {
  if (renorm) sim_renorm <- GMJMCMC:::marginal.probs.renorm(models)
  else sim_renorm <- GMJMCMC:::marginal.probs(models)
  rmse <- sqrt(mean((sim_renorm - full)^2))
  return(rmse)
}

rmse_conv <- function (full, sim, steps, renorm=T, lo=F) {
  rmse_converge <- matrix(NA, steps, 1)
  if (lo) {
    models <- c(sim$models, sim$lo.models)
    model.size <- length(models[[1]]$model)
    models.matrix <- matrix(unlist(models), ncol=model.size+3, byrow=T)
  } else models <- sim$models
  step_size <- length(models) / steps
  progress <- 0
  for(i in 1:steps) {
    models.use <- models[1:(i*step_size)]
    if (lo) models.use <- models.use[order(models.matrix[1:(i*step_size),(model.size+2)])]
    rmse_converge[i,] <- rmse(full, models.use, renorm)
    progress <- print.progressbar(progress, steps)
  }
  return(rmse_converge)
}

# Print a progress bar while iterating over a population
print.progressbar <- function (progress, size=40) {
  cat("\r", "|")
  for (p in 1:size-1) {
    if (progress >= p) cat("=")
    else cat(" ")
  }
  cat("|")
  return(progress+1)
}