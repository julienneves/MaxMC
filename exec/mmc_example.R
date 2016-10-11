## Example 1
## Exact Unit Root Test
library(fUnitRoots)

# Set seed
set.seed(123)

# Generate an double unit root AR(2) process with n = 50
y <- filter(rnorm(50), c(2,-1), method = "recursive")

# Set bounds for nuissance parameters
lower <- 0.8
upper <- 1.2

# Set the function to generate an AR(2) integrated process
dgp <- function(y, v) {
    ran.y <- filter(rnorm(length(y)), c(1-v,v),
                    method = "recursive")
}

# Set the Augmented-Dicky Fuller statistic
statistic <- function(y){
    out <- adfTest(y, lags = 2, type = "nc")
    return(out@test$statistic)
}

# Apply the MMC test
mmc(y, statistic = statistic , dgp = dgp, lower = lower,
    upper = upper, N = 99, type = "leq", method = "GenSA",
    control = list(max.time = 2))


## Example 2
## Exact Unit Root Test
library(MASS)

# Set seed
set.seed(123)

# Generate x1 ~ N(0,1) and x2 ~ N(0,100)
x1 <- rnorm(15, mean = 0, sd = 1)
x2 <- rnorm(25, mean = 0, sd = 10)
data <- list(x1 = x1, x2 = x2)

# Fit the sample using maximum likelihood
fit1 <- fitdistr(x1, "normal")
fit2 <- fitdistr(x2, "normal")

# Set the estimator value
est <- c(fit2$estimate, fit1$estimate["sd"])

# Set the bounds as the 99% confidence interval
lower <- est - 2.577 * c(fit2$sd, fit1$sd["sd"])
upper <- est + 2.577 * c(fit2$sd, fit1$sd["sd"])

# Set the DGP function under the null (i.e. mu_1=mu_2)
dgp <- function(data, v) {
    x1 <- rnorm(length(data$x1), mean = v[1], sd = v[3])
    x2 <- rnorm(length(data$x2), mean = v[1], sd = v[2])
    return(list(x1 = x1, x2 = x2))
}

# Set the statistic function
statistic <- function(data) {
    test <- t.test(data$x2, data$x1)
    return(test$statistic)
}

# Apply the test statistic
t.test(data$x2, data$x1)

# Apply the mmc procedure
mmc(y = data, statistic = statistic, dgp = dgp, est = est,
    lower = lower, upper = upper, N = 99,	type = "absolute",
    method = "pso")
