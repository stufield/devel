#' Complete denormalization wrapper
#'
#' Complete wrapper around functions and processes necessary
#' for full denormalization of ADATs
#' **Note:** we assume standard arguments for the normalization process
#' have been used. In particular, that the steps are:
#' hybNorm -> internal med norm of Calibrator|Buffer ->
#' Plate Scaling -> Calibration -> median normalization of the rest of the samples
#'
#' @param adat A fully normalized adat to be processed (multiple runs, usually),
#' should have a `PlateId` column.
#' @return A list of ADATs consisting of:
#' \item{raw }{Raw ADAT }
#' \item{hyb }{Internal hybridization }
#' \item{imed}{Internal median normalized (of only Calibrators and Buffers;
#'             as per V4 scheme).}
#' \item{plt }{Plate scaled}
#' \item{cal }{Calibrated}
#' \item{med }{Median normalized}
#' @author Stu Field, Eduardo Tabacman
#' @seealso \code{\link{normalizationWrapper}}
#' @references SomaLogic Platform BI
#' @examples
#' \dontrun{}
#' @export
denormalizationWrapper <- function(adat) {
  out <- list()
  out$med <- adat
  out$cal <- demedianNormalize(out$med, do_field = "SampleType", do_regexp = "QC|Sample")
  out$plt <- decalibrate(out$cal)
  out$imed <- deplateNormalize(out$plt)
  out$hyb <- demedianNormalize(out$imed, do_field = "SampleType", do_regexp = "Calibrator|Buffer")
  out$raw <- dehybNormalize(out$hyb)
  out
}
