#' Maximized Monte Carlo Simulation
#'
#' Generates N Monte Carlo replicates of a statistic applied to
#' data.
#'
#' @param v A vector paramaters. The vector \code{v} is use to
#'  specify the \code{dgp}. Note that if \code{dgp} is a
#'  function  of only \code{y} then we do not need to specify
#'  \code{v}. Default value is NULL.
#'
#' @inheritParams mmc
#'
#' @return The vector of replication of test statistic.
#'
#' @keywords internal
#'
simulation_mmc <- function(y, statistic,
                           dgp = function(y, v)
                               sample(y, replace = TRUE),
                           v, N, ...) {
  # Generate an empty array to store the simulated statistics
  S <- rep(NA, N)

  for (i in 1:N) {
    # Generate new observation y
    ran.y <- dgp(y, v)
    # Compute the statistic on this new observation y
    S[i] <- statistic(ran.y, ...)
  }
  return(S)
}

#' Monte Carlo Simulation
#'
#' Generates N Monte Carlo replicates of a statistic applied to
#' data.
#'
#' @inheritParams mc
#' @return The vector of replication of test statistic.
#'
#' @keywords internal
#'
simulation_mc <- function(y, statistic,
                          dgp = function(y)
                              sample(y, replace = TRUE),
                          N, ...) {
  # Generate an empty array to store the simulated statistics
  S <- rep(NA, N)

  for (i in 1:N) {
    # Generate new observation y
    ran.y <- dgp(y)
    # Compute the statistic on this new observation y
    S[i] <- statistic(ran.y, ...)
  }
  return(S)
}
