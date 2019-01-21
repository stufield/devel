# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
##############################
#         Function:   my.qq
##############################

#' Create Q-Q plots
#'
#' General tool for plotting unknown distributions against known distributions,
#' producing a histogram of the unknown data and a Quantile-Quantile plot of
#' the relationship.
#'
#' If \code{fun="norm"}, a Shapiro-Wilk normality test is performed and the
#' result added to the Q-Q plot.
#'
#' @param y The variable of interest as a numeric vector.
#' @param fun Which functional distribution you wish to test against. Options
#' are: \code{"norm"}, \code{"unif"}, \code{"exp"}, and \code{"binom"}.
#' @param na.rm Should \code{NA}s be removed from the data prior to analysis?
#' @param col Color of the Q-Q line to be plotted in the Q-Q plot.
#' @param lty Line type of the Q-Q line to be plotted in the Q-Q plot.
#' @param lwd Line width of the Q-Q line to be plotted in the Q-Q plot.
#' @param check.p.values Is \code{y} a vector of p.values?
#' @param ... Additional arguments passed to the \code{fun} function argument
#' above that describe the shape parameters of the quantile distributions,
#' typically \code{\link{qnorm}}, \code{\link{qunif}}, \code{\link{qexp}}, and
#' \code{\link{qbinom}}.
#' @return \item{plot1 }{A histogram of the raw data variable.} \item{plot2 }{A
#' Q-Q plot of the quantiles of the raw data and the theoretical distribution.}
#' \item{ShapiroTest }{If \code{fun="norm"}, a Shapiro-Wilk normality test is
#' performed and the result added to the Q-Q plot.}
#' @note %% ~~further notes~~
#' @author Stu Field
#' @seealso \code{\link{hist}} \cr \code{\link{qnorm}} \cr \code{\link{qunif}}
#' \cr \code{\link{qexp}} \cr \code{\link{qbinom}}
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#'
#' \dontrun{
#' test = rnorm(500)
#' my.qq(test, fun="norm")
#' my.qq(test, fun="unif")
#' my.qq(test, fun="exp")
#' test = rexp(500)
#' my.qq(test, fun="exp")
#' }
#'
my.qq <- function(y, fun=c("norm","unif","exp","binom"), na.rm=TRUE,
                  col=2, lty=3, lwd=1.5, ...) {

   var <- deparse(substitute(y))
   fun <- match.arg(fun)
   if ( na.rm ) y <- y[ !is.na(y) ]
   n <- length(y)
   xlabel <- deparse(substitute(y))
   if ( fun=='norm' ) {
      dist <- "Normal"
      normtest <- shapiro.test(y)
   }
   else if ( fun=='unif' )
      dist <- "Uniform"
   else if ( fun=='exp' )
      dist <- "Exponential"
   else if ( fun=='binom' )
      dist <- "Binomial"

   par(mfrow=c(1,2))
   hist(y, col=8, freq=FALSE, main=sprintf("Histogram of %s", var), xlab=var)

   qfun <- eval(parse(text=sprintf("q%s",fun)))
   vals <- seq_len(n) - 0.5
   x <- qfun(vals/n, ...)   # theoretical values

   par(pty="s")
   plot(x, sort(y), pch=19, col=8, ylab="Sample Quantiles",
        main=sprintf("%s Q-Q Plot", dist), xlab="Theoretical Quantiles")
   points(x, sort(y), pch=21, col=1)
   y2 <- quantile(y, c(0.25, 0.75))
   x2 <- qfun(c(0.25, 0.75), ...)
   slope <- diff(y2)/diff(x2)
   int <- y2[1] - slope * x2[1]
   abline(int, slope, col=col, lty=lty, lwd=lwd); grid()

   if ( fun=="norm" )
      addText(0.6, 0.2,
              sprintf("Shapiro W = %0.3f \n P = %0.4f", normtest$statistic, normtest$p.value), cex= 0.8)
}
#### ---- END FUNCTION ---- ####

# ---- created on: 2014-05-07 13:27:00
