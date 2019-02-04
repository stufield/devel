
#' Add assay Noise to CDF plot
#'
#' Add a CDF of the estimated assay variance via the Covance QC samples.
#'
#' @param apt Character. Which SOMAmer (or SeqId) to add. This queries the
#' \code{\link[SomaObjects]{mLod}} object to obtain the median absolute deviation
#' (MAD) for that aptamer. Ignored if `mad` is passed directly.
#' @param apt.median Numeric. The median value for the empirical values
#' to be plotted against. This is used to create a random normal of the
#' same median but with the assay variance from the median absolute
#' deviation (pinned shift).
#' @param matrix.type Which matrix type to look into the Covance set, currently
#' only Serum and Plasma.
#' @param ... Additonal arguments passed to \code{\link[SomaGlobals]{plotCDF}}
#' @author Stu Field
#' @seealso \code{\link[SomaPlyr]{matchSeqIds}}, \code{\link[SomaObjects]{mLod}},
#' \code{\link[stats]{rnorm}}, \code{\link[SomaGlobals]{plotCDF}}
#' @examples
#'
#' @importFrom rlang signal
#' @importFrom stats rnorm
#' @export addAssayVariance
addAssayVariance <- function(apt, apt.median, assay.var = NULL, mad = NULL,
                             matrix.type = c(NA, "plasma", "serum"), ...) {

  if ( apt.median > 10 ) {
    apt.median %<>% log10()
  }

  if ( is.null(assay.var) ) {
    if ( is.null(mad) ) {
      matrix.type <- match.arg(matrix.type)
      if ( is.na(matrix.type) ) {
        rlang::signal(
          "Must provide `matrix.type =` argument: is this plasma or serum?",
          "error")
      }
      mad <- mLod[[matrix.type]][ matchSeqIds(apt, rownames(mLod[[matrix.type]])), "madNQCreplicate"]
    }
    assay_sd <- 1.4826 * mad
  } else if ( is.null(mad) ) {
    assay_sd <- sqrt(assay.var)
  } else {
    rlang::signal(
      stringr::str_glue(
        "Unable to determine assay variance: provide \\
        either `assay.var =`, `mad =`, or pass neither."
        ), "error")
  }

  if ( is.finite(assay_sd) ) {
    plot.data <- stats::rnorm(100, mean = apt.median, sd = assay_sd)
    plot.data <- plot.data - median(plot.data) + apt.median      # to ensure pinned medians
    plotCDF(plot.data, add = TRUE, lwd = 2.5, do.log = FALSE, col = 1)
    plotCDF(plot.data, add = TRUE, lwd = 1.5, do.log = FALSE, ...)
  }

}

