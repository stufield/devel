#' Generic Bootstrapping Subroutine
#' 
#' Provide a numeric or character vector and create bootstrap samples 
#'   (with estimates) of the original vector. Also calculates CI95 
#'   using [quantile()].
#' 
#' @param x `numeric(n)`. The original data to be bootstrapped.
#'   Can also be character though unusual.
#' @param boot `integer(1)`. Number of bootstraps to perform.
#' @param FUN Function desired for the point estimate of the original data
#'   vector (if numeric data). Must return a scalar.
#' @param up `numeric(1)`. Upper confidence limit.
#' @param lo `numeric(1)`. Lower confidence limit.
#' 
#' @return A list containing:
#'   \item{BootSamples}{List of the bootstrap populations created during
#'     the simulation.}
#'   \item{BootEstimates}{List of the various point estimates of each of 
#'     the bootstrap samples.}
#'   \item{CI95}{Vector of the point estimate and upper & lower CI95 
#'     produced via the bootstrap samples.}
#'   \item{SE}{The standard error based on the original data.
#'     Could be used to calculate CI95 via 1.96*SE if so desired.}
#' 
#' @author Stu Field
#' @seealso [CI95se()], [quantile()]
#' 
#' @examples
#' bootstrap(x = round(runif(25,1,100)), boot = 50, FUN = mean) # numeric
#' bootstrap(x = LETTERS[1:26], boot = 50)                      # character
#' z <- factor(sample(c("stu", "is", "cool"), 10, replace = TRUE))
#' # bootstrap(x = z, boot = 50)      # factor (ask to convert to character)
#' @importFrom stats quantile var
#' @export
bootstrap <- function(x, boot = 1000, FUN, up = 0.975, lo = 0.025) {

  stopifnot(inherits(x, c("numeric", "character")))
  if ( is.numeric(x) ) {
    est <- FUN(x)  # calc pt estimate
    bootsamples <- replicate(boot, sample(x, replace = TRUE))
    boot_est <- sapply(bootsamples, FUN)        # pt. estimates of bootstraps
    upper    <- quantile(boot_est, probs = up)  # upper CI
    lower    <- quantile(boot_est, probs = lo)  # lower CI
    se       <- sqrt(var(boot_est))             # standard error of pt. estimate
    ret <- list(BootSamples   = bootsamples,
                BootEstimates = boot_est,
                CI95          = c(lower, estimate = est, upper),
                SE            = se)
  } else if ( is.character(x) ) {
    ret <- replicate(boot, sample(x, replace = TRUE))
  } else {
    stop(
      "`x` must be either numeric or character class: ", value(class(x)),
      call. = FALSE
    )
  }
  ret
}

