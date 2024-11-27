#' Add assay Noise to CDF plot
#'
#' Add a CDF of the estimated assay variance via the Covance QC samples.
#'
#' @param apt `character(1)`. Which feature to add. This queries the
#'   `splyr::mLod` object to obtain the median absolute deviation
#'   (MAD) for that aptamer. Ignored if `mad` is passed directly.
#' @param apt_median Numeric. The median value for the empirical values
#'   to be plotted against. This is used to create a random normal of the
#'   same median but with the assay variance from the median absolute
#'   deviation (pinned shift).
#' @param matrix_type Serum or Plasma.
#' @param ... Additonal arguments passed to [SomaPlotr::plotCDF()].
#' @author Stu Field
#' @importFrom stats rnorm
#' @export
addAssayVariance <- function(apt, apt_median, assay_var = NULL, mad = NULL,
                             matrix_type = c(NA, "plasma", "serum"), ...) {

  if ( apt_median > 10 ) {
    apt_median <- log10(apt_median)
  }

  if ( is.null(assay_var) ) {
    if ( is.null(mad) ) {
      matrix_type <- match.arg(matrix_type)
      if ( is.na(matrix_type) ) {
        stop(
          "Must provide `matrix_type =` argument: is this plasma or serum?",
          call. = FALSE)
      }
      mad <- mLod[[matrix_type]][matchSeqIds(apt, rownames(mLod[[matrix_type]])),
                                 "madNQCreplicate"]
    }
    assay_sd <- 1.4826 * mad
  } else if ( is.null(mad) ) {
    assay_sd <- sqrt(assay_var)
  } else {
    stop("Unable to determine assay variance: provide ",
         "either `assay_var =`, `mad =`, or pass neither.",
         call. = FALSE)
  }

  if ( is.finite(assay_sd) ) {
    plot_data <- stats::rnorm(100, mean = apt_median, sd = assay_sd)
    plot_data <- plot_data - median(plot_data) + apt_median      # to ensure pinned medians
    plotCDF(plot_data, add = TRUE, lwd = 2.5, do.log = FALSE, col = 1)
    plotCDF(plot_data, add = TRUE, lwd = 1.5, do.log = FALSE, ...)
  }
}

