#' P-value function
#'
#' Computes the p-value of the statistic by computing its rank
#' compared to its simulated values.
#'
#' We allow for three-type of p-value: \code{leq}, \code{geq},
#' \code{absolute} and \code{two-tailed}. For one-tailed test,
#' \code{leq} returns the proportion of simulated values smaller
#'  than the statistic while \code{geq} return the proportion of
#'  simulated values greater than the statistic. For two-tailed
#'  test, if the statistic is symmetric, one can use the
#'  absolute value of the statistic and its simulated value to
#'  retrieve a two-tailed test. If the statistic is not
#'  symmetric,
#'  one can specify the p-value type as \code{two-tailed} to
#'  obtained the p-value computed as the twice the minimum of
#'  \code{leq} and \code{geq}.
#'
#' Ties in the ranking are broken according to a uniform
#' distribution.
#'
#' @param S0 An atomic vector. Value of the test statistic
#' applied to the data.
#' @param S An atomic vector. Replication of the test statistic.
#' S must have length greater than one, with no missing values.
#' @param type A character string. Specifies the type of test
#' the p-value function produce. The possible values are
#' \code{geq}, \code{leq}, \code{absolute} and \code{two-tailed}.
#'  Default is \code{geq}.
#'
#' @return The p-value of S0 given the vector of replication S.
#' @export
#'
pvalue <- function(S0, S, type = c("geq", "leq",
                                    "absolute", "two-tailed")) {
    # Extract the number of simulation
    N <- length(S)
    # Combine S0 and S to obtain the total set where we rank S0
    set.S <- c(S0, S)

    # Take the absolute value of S0 and S if p-value type is "absolute"
    if (type == "absolute") {
        set.S <- abs(S)
    }

    # Rank S0 and S
    set.u <- runif(length(set.S[set.S == set.S[1]]))
    rank.S <- length(set.S[set.S < set.S[1]]) +
        length(set.u[set.u <= set.u[1]])

    # Compute the survival function
    survival.fun <- (N + 1 - rank.S[1])/N

    # Compute the p-value
    if (type == "absolute" || type == "geq") {
        pval <- (N * survival.fun + 1)/(N + 1)

    } else if (type == "leq") {
        pval <- (N * (1 - survival.fun) + 1)/(N + 1)

    } else if (type == "two-tailed") {
        pval <- 2 * min((N * (1 - survival.fun) + 1)/(N + 1),
                           (N * survival.fun + 1)/(N + 1))
    }
    return(pval)
}
