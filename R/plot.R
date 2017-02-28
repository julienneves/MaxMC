#' Print a Summary of a \code{mmc} Object
#'
#' This is a method for the function \code{print()} for objects of the
#' class \code{mmc}.
#'
#' @return The \code{mmc} object is returned invisibly.
#'
#' @inheritParams graphics::plot
#' @param x An object of class "mmc"
#' @export
#'
plot.mmc <- function(x, ...) {
    opt_monitor <- x$opt_monitor
    alpha <- x$alpha

    graphics::plot(opt_monitor$pval, xlab="Iterations",
         ylab="P-value", main = "Evolution of mmc", ...)
    graphics::lines(opt_monitor$max,col="green")

    if(!is.null(alpha)){
        lapply(alpha, function(alpha){graphics::abline(alpha,0,lty=2, col="red")})
    }

    invisible(x)
}
