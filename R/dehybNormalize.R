#' Undo hybNormalization
#'
#' Reverse the steps of hybNormalization
#' This involves dividing all the analytes for each sample by their
#' corresponding hyb. normalization scale factor
#'
#' @param adat A `soma.adat` object that has been hybnormalized and 
#' contains cscale factors among its columns
#' (i.e. a "HybControlNormScale" column)
#' @return A `soma.adat` object containing RFU values for each
#' analyte that corresponds to the data before the hyb. Normalization step
#' @author Eduardo Tabacman
#' @seealso \code{\link{SomaNormalization::hybNormalize}}
#' @examples
#' dehybNormalize(sample.adat)
#' @export
dehybNormalize <- function(adat) {
  if ( is.null(adat$HybControlNormScale) ) {
    stop("No hyb normalization scale factors detected in `adat`.",
         call. = FALSE)
  }
  snames <- getAptamers(adat)
  adat[,snames] <- adat[, snames]/adat$HybControlNormScale
  # erase hybNorm ScaleFactors
  adat$HybControlNormScale <- NULL
  adat
}
