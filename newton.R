
#' Hessian for logistic regression
logisticH <- function (theta, data, idx) {
  n <- length(idx)
  sub_data <- data[idx, ]
  y <- sub_data[, 1]
  x <- sub_data[, -1]
  eta <- 1/(1 + exp(-(x %*% theta)))
  hessian <- t(x) %*% diag(as.vector(eta * (1-eta))) %*% x
}

newtonMethod <- function (theta, data, idx, iter=1000) {
  for (i in seq_len(iter)) {
    gradient <- logisticG(theta, data, idx)
    hessian <- logisticH(theta, data, idx)
    theta <- theta - solve(hessian) %*% gradient
  }
  return(theta)
}

theta <- rep(0, 16)
idx <- 1:2000

result_irls <- glm.fit(data[,-1], data[,1], family=binomial())

result <- newtonMethod(theta, data, idx)

get_deviance(result_irls$coefficients, data[,-1], data[,1], binomial())
get_deviance(result, data[,-1], data[,1], binomial())

