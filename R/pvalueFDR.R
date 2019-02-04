
#' Calculate Benjamini-Hochberg FDR Values
#'
#' Diagnostic and discovery plots to explain how q-value calculations work via
#' Benjemani-Hochberg "step-up" correction procedure. FDR corrected p-values are
#' calculated from scratch and optionally returned.
#'
#' @param p Vector of p-values.
#' @param alpha Alpha level of significance.
#' @param plot Logical. Plot the p-values?
#' @param return.fdr Logical. Should q-values be returned in the same order as the
#' p-values were entered?
#' @return A data frame of the step-up procedure OR a vector of FDR-adjusted p-values
#' if \code{return.fdr=TRUE}. The data frame contains:
#' \item{p.value }{A sorted vector of the original p-values.}
#' \item{penalty (k/m)}{The threshold value, corresponding to k / m (slope).}
#' \item{threshold }{The threshold value, corresponding to alpha * k / m.}
#' \item{p_hat}{Term in the minimum function corresponding to p * m / k.}
#' \item{fdr }{The FDR-adjusted p-values.}
#' \item{alpha }{The chosen significance threshold.}
#' @author Stu Field
#' @note You're basically solving for the slope (k/m) that makes the p-value (alpha)
#' significant.
#' @seealso \code{\link[stats]{p.adjust}}, \code{\link{cummin}}
#' @references http://www.unc.edu/courses/2007spring/biol/145/001/docs/lectures/Nov12.html \cr
#' http://en.wikipedia.org/wiki/False_discovery_rate#Benjamini.E2.80.93Hochberg_procedure
#'
#' Benjamini, Y., and Hochberg, Y. (1995). Controlling the false
#' discovery rate: a practical and powerful approach to multiple
#' testing. *Journal of the Royal Statistical Society Series B*
#' \bold{57}, 289-300.
#'
#' @keywords FDR
#' @examples
#' p1 <- c(0.01, 0.013, 0.014, 0.19, 0.35, 0.5, 0.63, 0.67, 0.75, 0.81)
#' fdr1 <- pvalueFDR(p1, plot = TRUE)
#' fdr2 <- p.adjust(p1, method = "fdr")
#' all.equal(fdr1$fdr, fdr2)
#'
#' set.seed(1001)
#' p2 <- runif(10, 0.0005, 0.3)
#' pvalueFDR(p2, plot = TRUE)
#'
#' set.seed(666)
#' p3 <- c(runif(10), rep(0.5, 10))
#' pvalueFDR(p3, plot = TRUE)
#'
#' p4 <- c(runif(200), runif(25, 0.01, 0.1))
#' pvalueFDR(p4, plot = TRUE)
#' @importFrom magrittr "%>%"
#' @importFrom graphics abline grid legend
#' @importFrom rlang signal
#' @export pvalueFDR
pvalueFDR <- function(p, alpha = 0.05, plot = FALSE, return.fdr = FALSE) {

  if ( length(alpha) != 1 ) {
    rlang::signal("The `alpha =` argument must be a scalar value in [0, 1].",
                  "error")
  }

  m      <- length(p)
  if ( return.fdr ) {
    orig <- order(p)                   # get orig order
  }
  p_sort <- sort(p)                    # sorted p-values
  k_m    <- seq(m) / m                 # calc k/m; the penalty
  thresh <- k_m * alpha                # slope x alpha level
  p_hat  <- pmin(1L, p_sort * k_m^-1)  # calc adjusted p-values; max = 1.0
  # reverse order; calc cumulative min from hi -> lo; undo reverse
  fdr    <- p_hat %>% rev() %>% cummin() %>% rev()

  if ( plot ) {
    plot(k_m, p_sort, ylim = 0:1, xlim = 0:1,
         main = "P-values by Significance Index",
         ylab = "Sorted p-values", xlab = "Index (k/m)")
    grid(col = "gray70")
    abline(0, 1, col = 1, lwd = 1.5, lty = 2)
    slopes <- fdr %>% unique() %>% sort()
    cols   <- slopes %>% length() %>% rainbow()
    sapply(1:length(slopes), function(x) abline(0, slopes[x], col = cols[x]))
    abline(h = alpha/m, col = 4, lty = 2)         # add Bonferroni
    legend("topleft", legend = c("Bonferroni",
                               sprintf("FDR %0.2f%%", slopes * 100)),
           col = c(4, cols), cex = 0.8,
           lty = c(2, rep(1, length(slopes))),
           bg = NA, ncol = 2)
  }

  if ( return.fdr ) {
    fdr[ orig ]       # put back in orig order
  } else {
    data.frame(p.value = p_sort,
               penalty = k_m,
               threshold = thresh,
               p_hat = p_hat,
               fdr = fdr,
               alpha = ifelse(rev(cummax(rev(p_sort <= thresh))),
                            "signif.", "null")) %>%
    dplyr::rename("penalty (k/m)" = penalty)
  }

}

