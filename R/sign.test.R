# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
#####################

#' Calculate Sign Test
#'
#' Calculates a simple sign test for paired data whether the number of "ups" is
#' equal to the number of "downs". Tests the null probability P=0.5.
#'
#' @param x First vector of values, in order of individual.
#' @param y Second vector of values, in order of individual,
#' to be paired with `x`.
#' @param ... Additional arguments passed to \code{\link[stats]{binom.test}}.
#' @return The result of \code{\link[stats]{binom.test}} on the signs.
#' @author Stu Field
#' @seealso \code{\link[stats]{binom.test}}
#' @references Mick Crawley, The R Book, pg. 300.
#' @keywords ~kwd1 ~kwd2
#' @examples
#' sign.test(rnorm(10), rnorm(10))
#' vec1 <- rnorm(20)
#' s <- sample(1:20, 15)
#' vec2 <- vec1
#' vec2[s] <- vec2[s] + 1
#' vec2[setdiff(1:20,s)] <- vec2[setdiff(1:20,s)] - 1
#' sign.test(vec1, vec2)
#' @importFrom stats binom.test
#' @export sign.test
sign.test <- function(x, y, ...) {
  if ( length(x)!=length(y) ) {
    stop("Two vectors `x` and `y` must be same length.",
         call. = FALSE)
  }
  d <- x - y
  stats::binom.test(sum(d > 0), length(d), ...)
}
