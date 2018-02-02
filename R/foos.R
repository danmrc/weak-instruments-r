#' Cragg Donald Statistics
#'
#' @description Computes de Cragg Donald Statistics
#' @param endogenous The endogenous variables, a n by k matrix.
#' @param instruments The Instruments, a n by z matsrix.
#' @return The Cragg Donald Statistics
#' @note The endogenous variables and instruments should be the columns of the matrix

cragg_donald_stats <- function(endogenous,instruments){
  residuals <- matrix(0, ncol = ncol(endogenous), nrow = nrow(endogenous))
  for (i in 1:ncol(endogenous)) {
    mod.aux <- lm(endogenous[,i] ~ instruments)
    residuals[,i] <- resid(mod.aux)
  }
  Pz <- instruments %*% solve(t(instruments) %*% instruments) %*% t(instruments)
  Sv <- cov(residuals)
  sqrt.Sv <- chol(Sv)
  inv.Sv <- solve(sqrt.Sv)
  G <- t(inv.Sv) %*% t(endogenous) %*% Pz %*% endogenous %*% inv.Sv/ncol(instruments)
  g <- min(eigen(G)$values)
  return(g)
}

#'Stock and Yogo Test
#'
#' @description Computes the Stock and Yogo test for weak instruments detection, with critical values
#' @param X The Endogenous variables
#' @param Z The Instruments
#' @param bias The maximum bias accept. Current values are 0.05,0.1,0.2,0.3. Default is 0.1.
#' @return The value of the Donald Cragg Statistics and the critical value for the number of endogenous and the      number of instruments
#' @section Warning: Only work until 30 instruments or 3 endogenous variables

stock_yogo_test <- function(X,Z,bias = 0.1){
  load("data/stock_watson_critical.Rdata")
  K <- ncol(Z)
  E <- ncol(X)
  if (K < (E + 2)) {
    stop("Not enough instruments")
  }
  if (E > 3) {
    warning("No avaiable value for more than 3 endogenous variables. Showing critical value for 3.")}
  stat <- cragg_donald_stats(X,Z)
  E.aux <- ifelse(E > 3,3,E)
  crit_val <- crit_val[[E.aux]]
  row <- which(crit_val[,1] == K)
  crit_val <- crit_val[row,as.character(bias)]
  return(list("stat" = stat, "critical_value" = crit_val))
}

#'AR Test
#'
#' @description Hypotesis test that do not depends if the instrument is weak or strong
#' @param y The dependent variable of the second stage
#' @param X The Endogenous variables
#' @param Z The Instruments
#' @param beta0 The vector of the coefficients to be tested
#' @param intercept Logical. Should an intercept be included? Default is false.
#' @note Works with more than one endogenous variable.
#' If used with the coefficients from TSLS, it computes the J-statistic

ar_test <- function(y,X,Z,beta0, intercept = F){
  if (length(beta0) == 1) {
    beta0 <- matrix(beta0,ncol = 1)
  }
  if (intercept == T) {
    X <- cbind(1,X)
  }
  k <- ncol(Z)
  n <- nrow(Z)
  u <- y - X %*% beta0
  Pz <- Z %*% solve(t(Z) %*% Z) %*% t(Z)
  Mz <- diag(nrow = nrow(Pz)) - Pz
  AR_num <- t(u) %*% Pz %*% u/k
  AR_den <- t(u) %*% Mz %*% u/(n - k)
  AR_stat <- AR_num/AR_den
  p_val <- pchisq(k*AR_stat, df = k, lower.tail = F)
  return(list("stat" = AR_stat,"p-val" = p_val))
}

