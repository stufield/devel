#' Undo calibration
#'
#' Reverse the steps of calibration.
#' This involves dividing the analytes in each plate by their
#' corresponding scale factor
#'
#' @param adat A `soma.adat` object that has been calibrated and contains calibration scale factors in its attributes
#' @param ... Additional arguments passed to \code{\link{Somanormalization::getCalSFs}},
#' typically an "apt.data" object if `adat` has been modified
#' @return A `soma.adat` object containing RFU values for each
#' analyte that corresponds to the data before the calibration step
#' @author Eduardo Tabacman
#' @seealso \code{\link{getCalSFs}}, \code{\link{calibrate.adats}}
#' @examples
#' decalibrate(sample.adat)
#' @importFrom SomaReadr cleanNames
#' @importFrom SomaNormalization getCalSFs
#' @export dehybNormalize
decalibrate <- function(adat, ...) {

  calsfs <- getCalSFs(adat, ...)
  # get the same names for each sample in the adat
  plate_names <- cleanNames(sprintf("Cal.Set.%s", adat$PlateId))

  # remove calsfs from attributes
  tmp_ad <- attributes(adat)
  tmp_ad$Col.Meta[unique(plate_names)] <- NULL

  for (pn in unique(plate_names)) {
    adat[plate_names == pn, calsfs$AptName] <- t(t(adat[plate_names == pn, calsfs$AptName])/calsfs[[pn]])
  }

  attributes(adat) <- tmp_ad

  return(adat)
}
