# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-10-25

# Function for running a set of models
run_sim <- function (x, y, loglik_fun, nobs, models, ...) {
  worker_id <- (models[1]-1) / length(models)
  models <- models[!is.na(models)]
  cat(paste0("Worker: ", worker_id, " simulating ", length(models), " models.\n"))
  res <- vector("list", length(models))
  progress <- 0
  index <- 1
  for (i in models) {
    modelvector <- as.logical(c(T,intToBits(i)[1:15]))
    loglik <- loglik_fun(y[1:nobs], x[1:nobs,], modelvector, ...)
    res[[index]] <- list(prob=NA, model=modelvector[-1], crit=loglik, alpha=NA)
    if (index %% max(floor(length(models)/100),1) == 0) {
      progress <- print.progressbar(progress, 100)
      cat(worker_id)
      gc()
    }
    index <- index + 1
  }
  cat(paste0("\n","Worker: ", worker_id, " done simulating ", length(models), " models.\n"))
  return(res)
}

# Function for running multiple sets of models with multiple threads
run_multisim <- function (x, y, loglik, model_parts, n_obs, name, directory, ...) {
  dir.create(directory)
  cat(paste0("\n", "Running ", name, " simulation.\n"))
  simres <- mclapply(seq_len(nrow(model_parts)), function (mods) {
    run_sim(x, y, loglik, n_obs, model_parts[mods,], ...)
  }, mc.cores = nrow(model_parts), mc.preschedule = F)
  simres <- unlist(simres, recursive = F)
  assign(name, simres)
  filename <- paste0(directory,"/",name,".Rdata")
  eval(parse(text=paste0("save(",name,", file=\"",filename,"\")")))
  cat(paste0("\n", name, " simulation done.\n"))
}

