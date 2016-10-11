#' Monte Carlo with Tie-Breaker
#'
#' Generates N Monte Carlo replicates of a statistic applied to
#' data.
#'
#' The \code{ran.gen} function defined by the user is used to
#' generate new observations in order to compute the simulated
#' statistics.
#'
#' Then \code{\link{pvalue}} is applied to the statistic and
#' its simulated values. \code{\link{pvalue}} computes the
#' p-value by ranking the statistic compared to its simulated
#' values. Ties in the ranking are broken according to a uniform
#' distribution.
#'
#' We allow for three-type of tests: \code{one-tailed},
#' \code{absolute}, \code{two-tailed}. The \code{one-tailed}
#' test simply returns the p-value of the statistic at the right
#' end of the distribution. If the statistic is symmetric, one
#' can use the absolute value of the statistic and its simulated
#' value to retrieve a two-tailed test. If the statistic is not
#' symmetric, one can specify the type as \code{two-tailed} to
#' obtained the p-value computed at both end of the distribution.
#'
#' @param dgp A function. The function inputs the first argument
#'  \code{y}, and outputs a simulated \code{y} (i.e. an object
#'  of the same type as \code{y}). It is analogous to the data
#'  generating process under the null. Default value is
#'  sample(y, replace = TRUE), the bootstrap resampling of
#'   \code{y}.
#'
#' @inheritParams mmc
#' @inheritParams pvalue
#'
#' @return The returned value if an object of class "mc",
#' containing the following components:
#'  \item{S0}{Observed value of \code{statistic}}
#'  \item{pval}{Monte Carlo p-value of \code{statistic}}
#'  \item{y}{Data specified in call}
#'  \item{statistic}{\code{statistic} function specified in call}
#'  \item{dgp}{\code{dgp} function specified in call}
#'  \item{N}{Number of replication specified in call}
#'  \item{type}{\code{type} of p-value specified in call}
#'  \item{call}{Original call to \code{mmc}}
#'  \item{seed}{value of \code{.Random.seed} at the start of
#'  \code{mc} call}
#'
#' @export
#'
#' @example \exec\mc_example.R
#'
mc <- function(y, statistic, ..., dgp = function(y) sample(y, replace = TRUE),
                       N = 99, type = c("geq", "leq", "absolute", "two-tailed")) {
  # Match type and extract exact call
  type <- match.arg(type)
  call <- match.call()

  # Extract function from string of characters in statistic
  if (is.character(statistic)) {
    statistic <- get(statistic, mode = "function", envir = parent.frame())
  }
  # Test if statistic is a function
  if (!is.function(statistic)) {
    stop("'statistic' must be a function or a string naming
         a valid function")
  }

  # Generate seed if none is specified in GlobalEnv
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)){
    runif(1)
  }
  # Extract seed integer vector
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  # Compute the statistic
  S0 <- statistic(y, ...)

  # Test if S0 is a vector of length 1
  if (length(S0)!=1 && !is.atomic(S0)){
      stop("'statistic' must return an atomic vector of
           length = 1")
  }

  # Simulate the statistic N times
  S <- simulation_mc(y, statistic, dgp, N, ...)
  # Compute the p-value
  pval <- pvalue(S0, S, type)

  return_mc(S0 = S0, y = y, statistic = statistic, dgp = dgp,
            N = N, type = type, call = call, seed = seed, pval= pval)
}

