#' Format the optimization method controls
#'
#' This function provides a way to merge the user specified controls for the
#' optimization methods with their respective default controls.
#'
#' @param opt_monitor A data.frame
#' @inheritParams mmc
#'
#' @return A list. Arguments to be used to control the behavior
#' of the algorithm chosen in \code{method}.
#'
#' @keywords internal
#'
monitor_mmc <- function(opt_monitor, alpha, monitor = TRUE){

    if(monitor==FALSE){
        return()
    } else {
        opt_monitor<- opt_monitor[!is.na(opt_monitor),]
        current <- opt_monitor[length(opt_monitor),]
        plot(opt_monitor$ite,opt_monitor$pval,
             xlab="Iterations", ylab="P-value", main = "Evolution of mmc p-value")
        lines(opt_monitor$max,col="red")
        cat("Iteration", current$ite, "| Current", current$pval,
                "| Best", current$max, "\r")
        flush.console()
    }
}
