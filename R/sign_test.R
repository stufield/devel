
#' Calculate Sign Test
#'
#' Calculates a simple sign test for paired data
#' whether the number of "ups" is equal to the number 
#' of "downs". Tests the null probability `p = 0.5`.
#'
#' @param x First vector of values, in order of individual.
#' @param y Second vector of values, in order of individual,
#' to be paired with `x`.
#' @param ... Additional arguments passed to \code{\link[stats]{binom.test}}.
#' @return The result of \code{\link[stats]{binom.test}} on the signs.
#' @author Stu Field
#' @seealso \code{\link[stats]{binom.test}}
#' @references Mick Crawley, The R Book, pg. 300.
#' @examples
#' sign.test(rnorm(10), rnorm(10))
#' vec1 <- rnorm(20)
#' s <- sample(1:20, 15)
#' vec2 <- vec1
#' vec2[s] <- vec2[s] + 1
#' vec2[setdiff(1:20, s)] <- vec2[setdiff(1:20, s)] - 1
#' sign.test(vec1, vec2)
#' @importFrom stats binom.test
#' @importFrom rlang signal
#' @export sign_test
sign_test <- function(x, y, ...) {
  if ( length(x) != length(y) ) {
    rlang::signal("Two vectors `x` and `y` must be same length.", "error")
  }
  d <- x - y
  stats::binom.test(sum(d > 0), length(d), ...)
}
