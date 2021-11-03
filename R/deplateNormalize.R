#' Reverts Plate Scale Normalization
#'
#' Undoes plate scale normalization on an adat, which should have have PlateId
#' and `PlateScale_Scalar_<PlateId>` field in the Header.Meta$HEADER attributes
#'
#' @param adat A palte scaled adat
#' @author Eduardo Tabacman
#' @seealso [SomaNormalization::plateNormalize()]
#' @references Darryl Perry's Plate Shift Normalization
#' @return A de-plate normalized adat
#' @examples
#' orig_adat <- deplateNormalize(adat)
#' @export
deplateNormalize <- function(adat) {
  stopifnot("PlateId"%in%names(adat))
  # get the values for Plate SF
  psfs <- getPlateScale_Scalar(adat)
  # extract the plate ids
  pids <- gsub("PlateScale_Scalar_(.*)", "\\1", names(psfs))
  # undo scaling
  snames <- getAptamers(adat)
  for (i in seq(pids)) {
    adat[adat$PlateId==pids[i], snames] <- adat[adat$PlateId==pids[i], snames]/psfs[[i]]
  }
  # delete attributes and return
  removePlateScale_Scalar(adat)
}

#' auxiliary function to get at the scale factors in the attributes of the adats
#'
#' @param adat an adat with PlateId and PlateScale_Scalars in its attributes
#' @return a list of PlateScalar_Scale factors, indexed by the
#' attribute names: `PlateScale_Scalar_<PlateId>`
getPlateScale_Scalar <- function(adat) {
  stopifnot("PlateId"%in%names(adat))
  # build names according to the PX scheme
  platescale_ns <- unique(paste0("PlateScale_Scalar_", adat$PlateId))
  headerAtts <- attributes(adat)$Header.Meta$HEADER
  stopifnot(all( platescale_ns %in% names(headerAtts)))
  headerAtts[platescale_ns]
}

#' auxiliary function to remove the scale factors in the attributes of the adats
#'
#' @param adat an adat with PlateId and PlateScale_Scalars in its attributes
#' @return the same adat, but with the attributes corresponding
#' to PlateScaling removed.
removePlateScale_Scalar <- function(adat) {
  stopifnot("PlateId"%in%names(adat))
  # build names according to the PX scheme
  orig_attrs <- attributes(adat)
  platescale_ns <- unique(paste0("PlateScale_Scalar_", adat$PlateId))
  headerAtts <- orig_attrs$Header.Meta$HEADER
  stopifnot(all( platescale_ns %in% names(headerAtts)))
  orig_attrs$Header.Meta$HEADER[platescale_ns] <- NULL
  attributes(adat) <- orig_attrs
  adat
}

