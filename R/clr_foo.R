#' Conditional Likelihood Ratio
#' 
#' @description Computes the conditional likelihood ratio. Base for the CLR test
#' @seealso \code{\link{clr_test}}

clr <- function(y,x,Z,H,beta0){
  mod.1 <- lm(y ~ Z)
  mod.2 <- lm(x ~ Z)
  Y <- cbind(y,x)
  U <- cbind(resid(mod.1),resid(mod.2))
  Sigma <- var(U)
  W <- solve(Sigma)
  sqrt.Z <- solve(chol(t(Z) %*% Z))
  b0 <- c(1,-beta0)
  a0 <- c(beta0,1)
  right_matrix_S <- solve(chol(t(b0) %*% Sigma %*% b0))
  right_matrix_T <-  solve(chol(t(a0) %*% W %*% a0))
  S <- sqrt.Z %*% t(Z) %*% Y %*% b0 %*% right_matrix_S
  big.T <- sqrt.Z %*% t(Z) %*% Y %*% W %*% a0 %*% right_matrix_T
  ST <- cbind(S,big.T)
  Q <- t(ST) %*% ST
  LR <- 0.5*(Q[1,1] - Q[2,2] + sqrt((Q[1,1] + Q[2,2])^2 - 4*(Q[1,1]*Q[2,2] - Q[1,2]^2)))
  return(list("Likelihood Stat" = LR, "Qt" = Q[2,2])) 
}

p_fun_integral <- function(s,m,qt,k){
  x <- (qt+m)/(1+qt*s^2/m)
  cdf_chi <- pgamma(x/2,k/2)
  return(cdf_chi*(1-s^2)^((k-3)/2))
}

clr_pvalue <- function(lr,qt,k){
  K <- gamma(k/2)/(sqrt(pi)*gamma((k-1)/2))
  integral <- integrate(p_fun_integral, lower = 0, upper = 1, lr,qt,k)
  return(1 - 2*K*integral$value)
}

#' CLR Test
#' 
#' @description This is an implementation of Moreira (2003)
#' @param y The dependent variable of the second stage
#' @param x The endogenous variable
#' @param Z The matrix of instruments
#' @param H The matrix of exogenous variables (not implemented yet)
#' @param beta0 The hypotesis to be tested
#' @note Does only work with one endogenous variable
#' @return The LR value and the p-value for the test. 

clr_test <- function(y,x,Z,H,beta0){
  clr_val <- clr(y,x,Z,H,beta0)
  LR <- clr_val[[1]]
  p_val <- clr_pvalue(LR,clr_val[[2]],ncol(Z))
  return(structure(list("LR value" = LR,"p-value" = p_val), class = "moreiratest"))
}
