# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
###################################
#      Function:   computeCVBands
###################################

#' Compute CV Bands
#'
#' Calculate random variations for the passed RFU range using
#' the specified CV level. Computation is repeated [iter=]
#' times, and the empirical CI limits returned.
#'
#' @param rfu.range RFU (linear) values for which CV bands will be calculated
#' Best if this is something like seq(100,1000,10).
#' @param cv The CV as decimal (i.e. 5\% CV=0.05)
#' @param iter Integer. Number of iterations of random adjustment.
#' @param conf Numeric. The empirical confidence interval as decimal (i.e. 95\% CI=0.95).
#' @return A data frame with three columns:\cr
#' \item{rfu}{the rfu values from input parameters}
#' \item{upper}{the corresponding upper eCI band for each RFU}
#' \item{lower}{the corredspoding lower eCI band for each RFU}
#' @note Empirical estimation.
#' @author Kirk DeLisle
#' @examples
#'
#' @export computeCVBands
computeCVBands <- function(rfu.range, cv, iter = 1000, conf = 0.95) {
  cv_bands <- sapply(rfu.range, function(rfu) {
                       smpl <- rfu + rnorm(iter, mean = 0, sd = rfu * cv)
                       c(rfu = rfu, quantile(smpl, p = c((1 - conf) / 2, 1 - (1 - conf) / 2)))
      })
  as.data.frame(t(cv_bands))
}


#' @describeIn computeCVBands
#' Calculate confidence interval bands. Analytical estimation.
#' @author Stu Field
#' @export calcCVbands
calcCVbands <- function(rfu.range, cv, conf = 0.95) {
  sapply(rfu.range, function(rfu) {
         lower <- qnorm((1 - conf) / 2, mean = rfu, sd = rfu * cv)
         upper <- qnorm((1 - conf) / 2, mean = rfu, sd = rfu * cv,
                        lower.tail = FALSE)
         c(rfu = rfu, loCI95 = lower, upCI95 = upper)
    }) %>%
  t() %>% data.frame()
}

