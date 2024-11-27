#' Create Normalization Summary Table
#'
#' This function summarizes the normalization and calibration metrics
#'   into a table. It includes the plate normalization if performed
#'   and the hyb and med normalization scale factor ranges. This
#'   function is primarily used in the 3 plate reproducibility study
#'   experimental desin, but could be used for any normalized ADAT.
#'
#' @param A median normalizaed and calibrated ADAT, with multiple
#'   plates that have been calibrated into one data frame ("soma.adat").
#'
#' @return A summary table of the calibration results
#' @author Stu Field
#'
#' @examples
#' \dontrun{
#' createNormalizationSummaryTable(dat)
#' }
#' @export
createNormalizationSummaryTable <- function(dat) {

  if ( all(!is.na(as.numeric(dat$PlateId))) ) {   # catch for numeric plateIds w/o leading zeros
    levs         <- as.numeric(dat$PlateId) %>% unique() %>% sort()
    dat$PlateId %<>% factor(levels = levs)
  }

  spldata  <- split(dat, dat$PlateId)
  hybnorms <- lapply(spldata, function(.x) {
                       hyb <- range(.x$HybControlNormScale)
                       hyb <- sprintf("(%0.2f, %0.2f)", hyb[1], hyb[2])
                       data.frame(a = hyb) %>% stats::setNames("Hyb SF")
    }) %>% do.call(rbind, .)

  if ( "PlateScale_Scalar" %in% names(dat) ) {
    hybnorms[["Plt SF"]] <- tapply(dat$PlateScale_Scalar, dat$PlateId, function(x) x[1]) %>% round(2)
  } else if ( "PlateScale_Scalar"%in%names(atts <- attributes(dat)$Header.Meta$HEADER) )
    hybnorms[["Plt_SF"]] <- atts[ names(atts) ] %>% unlist %>% round(2)

  mednorms <- lapply(spldata, function(.x) {
                       med <- range(.x[, getNormNames(.x)])
                       med <- sprintf("(%0.2f, %0.2f)", med[1], med[2])
                       data.frame(b = med) %>% stats::setNames("Med SF")
    }) %>% do.call(rbind, .)

  sop        <- calibrationSOP(dat, do.plot = FALSE, verbose = FALSE)$cal.table
  sop        <- sop[, names(sop) != "Median Shift"] %>% round(2)
  names(sop) <- c("Cal SF", "Tails_%", "Tails_#")
  cbind(hybnorms, mednorms, sop)
}
