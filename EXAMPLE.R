## Behrens-Fisher Problem
library(MASS)
library(mmc)

bf_norm <- function(delta, rho, N1, N2, alpha) {
  # Generate sample x1 ~ N(0,1) and x2 ~ N(0,4)
  x1 <- rnorm(N1, mean = 0, sd = 1)
  x2 <- rnorm(N2, mean = delta, sd = rho)
  data <- list(x1 = x1, x2 = x2)

  # Extract the estimate for the nuisance parameters p = sigma_2/sigma_1
  est <- sd(x2)
  lower <- 0.8 * est
  upper <- 1.2 * est

  # Set the function for the DGP under the null (i.e. two population means are equal)
  dgp <- function(data, p) {
      x1 <- rnorm(length(data$x1), mean = 0, sd = 1)
      x2 <- rnorm(length(data$x2), mean = 0, sd = p)
      return(list(x1 = x1, x2 = x2))
  }

  # Set the statistic function to Welch's t-test
  welch <- function(data) {
      test <- t.test(data$x2, data$x1)
      return(test$statistic)
  }

  # Apply Welch's t-test
  out_t <- t.test(data$x2, data$x1)

  # Apply the mmc procedure
  out_mmc <- mmc(y = data, statistic = welch, dgp = dgp, est = est,
                 lower = lower, upper = upper, N = 99,	type = "absolute",
                 method = "pso", alpha = alpha)
  return(c(out_t$p.value,out_mmc$pval))
}

bf_t <- function(delta, rho, df, N1, N2, alpha) {
    # Generate sample x1 ~ N(0,1) and x2 ~ N(0,4)
    x1 <- rt(N1, df = df, ncp = delta)
    x2 <- rho * rt(N2, df = df)
    data <- list(x1 = x1, x2 = x2)

    # Extract the estimate for the nuisance parameters p = sigma_2/sigma_1
    est <- sd(x2)
    lower <- 0.8 * est
    upper <- 1.2 * est

    # Set the function for the DGP under the null (i.e. two population means are equal)
    dgp <- function(data, p) {
        x1 <- rt(N1, df = df)
        x2 <- p * rt(N2, df = df)
        return(list(x1 = x1, x2 = x2))
    }

    # Set the statistic function to Welch's t-test
    welch <- function(data) {
        test <- t.test(data$x2, data$x1)
        return(test$statistic)
    }

    # Apply Welch's t-test
    out_t <- t.test(data$x2, data$x1)

    # Apply the mmc procedure
    out_mmc <- mmc(y = data, statistic = welch, dgp = dgp, est = est,
                   lower = lower, upper = upper, N = 99,	type = "absolute",
                   method = "pso", alpha = alpha)
    return(c(out_t$p.value,out_mmc$pval))
}


repl <- 250
alpha <- 0.05

data <- expand.grid(delta = c(0), rho = c(1,5,10,100),
                    df = c(1,2,3), N1 = c(10,25,1000), N2 = c(10,25,1000))

test <- function(data){
    S <- replicate(repl, bf_t(data$delta, data$rho, data$df, data$N1, data$N2, alpha))
    level <- rowSums(S <= alpha)/repl * 100
    return(level)
}

S <- sapply(1:2, function(x) test(data[x,]))

