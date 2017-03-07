#' Print a Summary of a \code{mmc} Object
#'
#' This is a method for the function \code{print()} for objects of the
#' class \code{mmc}.
#'
#' @return The \code{mmc} object is returned invisibly.
#'
#' @param x An object of class "mmc"
#' @param ... Arguments to be passed to methods, such as \link{graphical parameters} (see \code{\link{par}}).
#' @export
#'
plot.mmc <- function(x, ...) {
    # Extract information from mmc object
    opt_trace <- x$opt_trace
    alpha <- x$alpha

    graphics::plot(opt_trace$pval, xlab="Iterations",
         ylab="P-value", main = "Evolution of mmc", ...)
    graphics::lines(opt_trace$max,col="green")

    # Add line if level is specified
    if(!is.null(alpha)){
        lapply(alpha, function(alpha){graphics::abline(alpha,0,lty=2, col="red")})
    }

    invisible(x)
}
