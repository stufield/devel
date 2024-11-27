#' Calculate SEM 95% Confidence Intervals
#'
#' Uses standard error (of the mean) calculation to
#'   determine the 95% CI of a vector of data.
#'   Does not use a bootstrapping of empirical data,
#'   but the Gaussian approximation.
#'
#' @param x `numeric(n)`. A vector to calculate the CI95.
#'
#' @return A numeric vector with 3 named entries:
#'   \item{lower}{the lower CI95.}
#'   \item{mean}{the arithmetic mean.}
#'   \item{upper}{the upper CI95.}
#' @author Stu Field
#' @seealso [bootstrap()] for CI95 via bootstrapping.
#' @examples
#' CI95se(rnorm(100))
#' @importFrom stats sd
#' @export
CI95se <- function(x) {
  sem   <- sd(x) / sqrt(length(x))
	shift <- 1.96 * sem
  c(lower = mean(x) - shift, mean = mean(x), upper = mean(x) + shift)
}
