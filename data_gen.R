u <- rnorm(200)

z.1 <- rnorm(200)
z.2 <- rnorm(200)
z.3 <- rnorm(200)
z.4 <- rnorm(200)

x.1 <- u + 0.5*z.1 + z.2 + z.4 + rnorm(200)
x.2 <- u + 0.4*z.1 - 0.9*z.2 + 2*z.3 + rnorm(200)

y <- 1 + u + 2*x.1 - 2*x.2 + rnorm(200)

lm(y ~ x.1 + x.2)

strong_data <- data.frame(z.1,z.2,z.3,z.4,x.1,x.2,y)

X <- cbind(x.1,x.2)
Z <- cbind(z.1,z.2,z.3,z.4)

stock_yogo_test(X,Z)

library(AER)

aa <- ivreg(y ~ x.1 + x.2|z.1+z.2+z.3+z.4)

x.1 <- 2*u + 0.1*z.1 - 0.02*z.2 + rnorm(200)
x.2 <- 2*u + 0.0074*z.4 -0.09*z.2 - 0.012*z.3 + rnorm(200)

y <- 1 + u + 2*x.1 - 2*x.2 + rnorm(200)

X <- cbind(x.1,x.2)
Z <- cbind(z.1,z.2,z.3,z.4)

stock_yogo_test(X,Z)

lm(y ~ x.1 + x.2)
ivreg(y ~ x.1+x.2|z.1+z.2+z.3+z.4)

weak_data <- data.frame(z.1,z.2,z.3,z.4,x.1,x.2,y)

save(weak_data,file = paste0(getwd(),"/data/weak_data.Rdata"))
save(strong_data,file = paste0(getwd(),"/data/strong_data.Rdata"))
