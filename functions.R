# Plot many columns in a matrix, log scale can be enabled too
multiplot <- function (mat, logscale=F, ylim=c(min(mat), max(mat)), legend=F, names=names(mat), ...) {
  if (logscale) {
    mat[mat > 0] <- log(mat[mat > 0])
    mat[mat < 0] <- -log(-mat[mat < 0])
  }
  mat <- as.matrix(mat)
  rbcol <- rainbow(ncol(mat))
  plot(mat[,1], type="l", ylim=ylim, col=rbcol[1], ...)
  if (ncol(mat) > 1) for (i in 2:ncol(mat)) lines(mat[,i], col=rbcol[i])
  if (legend) {
    if (is.null(names)) names <- 1:ncol(mat)
    legend("topright", col=rbcol, legend=names, lty=1)
  }
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

# Function to align model matrix with the number of cores available
align_models <- function (models) {
  num_cores <- detectCores()
  if ((length(models) %% num_cores) != 0) {
    models <- c(models, rep(NA, (num_cores-(length(models) %% num_cores))))
  }
  model_partitions <- matrix(models, num_cores, byrow=T)
  return(model_partitions)
}

