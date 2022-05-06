# 逻辑回归的对数似然函数
logliklogit <- function(beta) {
  Xb <- X %*% beta
  log1Xb <- log(1 + exp(Xb))
  loglik <- (t(y) %*% Xb) - sum(log1Xb)
  return(loglik)
}

gradlogit <- function(beta) {
  Xb <- X %*% beta
  lamXb <- 1 / (exp(-Xb) + 1)
  grad <- t(X) %*% (y - lamXb)
  return(grad)
}

hesslogit <- function(beta) {
  Xb <- X %*% beta
  weight <- exp(Xb) / (1 + exp(Xb))^(-2)
  weightm <- diag(as.vector(weight))
  hess <- -t(X) %*% weightm %*% X
  return(hess)
}