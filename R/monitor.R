#' Format the optimization method controls
#'
#' This function provides a way to merge the user specified controls for the
#' optimization methods with their respective default controls.
#'
#' @param ite A
#' @param pval A
#' @inheritParams mmc
#'
#' @return A list. Arguments to be used to control the behavior
#' of the algorithm chosen in \code{method}.
#'
#' @keywords internal
#'
monitor_mmc <- function(ite, pval, monitor){
    if(monitor==TRUE){
        if(ite==1){
            plot(NULL, xlim=c(1,1000), ylim=c(0,1),
                 xlab="Iterations", ylab="P-value", main = "Evolution of mmc p-value")
            points(ite,pval)
            cat("Maximized Monte Carlo\n")
            cat("Iteration", ite, "| P-value", pval, "\r")
            flush.console()
        }
        else{
            points(ite,pval)
            cat("Iteration", ite, "| P-value", pval,"\r")
            flush.console()
        }
    }
}
