
#' Create Normalization Summary Table for Urine
#'
#' This function summarizes the normalization metrics
#' into a table. It includes the plate normalization if performed
#' and the hyb and med normalization scale factor ranges. This
#' function is primarily used in the 3 plate reproducibility study
#' experimental design, but could be used for any normalized ADAT.
#'
#' @param A median normalizaed and calibrated ADAT, with multiple
#' plates that have been normalized into one data frame ("soma.adat").
#' @return A summary table of the normalization results
#' @author **Stu Field**, **Eduardo Tabacman**
#' @seealso \code{\link[SomaNormalization]{sop.calibration}}
#' @examples
#' \dontrun{
#' createUrineNormalizationSummaryTable(dat)
#' }
#' @importFrom purrr set_names
#' @export createUrineNormalizationSummaryTable
createUrineNormalizationSummaryTable <- function(dat) {

  if ( all(!is.na(as.numeric(dat$PlateId))) ) {   # catch for numeric plateIds w/o leading zeros
    levs         <- as.numeric(dat$PlateId) %>% unique() %>% sort()
    dat$PlateId %<>% factor(levels = levs)
  }

  spldata  <- split(dat, dat$PlateId)
  hybnorms <- lapply(spldata, function(.x) {
                       hyb <- range(.x$HybControlNormScale)
                       hyb <- sprintf("(%0.2f, %0.2f)", hyb[1], hyb[2])
                       data.frame(a = hyb) %>% purrr::set_names("Hyb SF")
    }) %>% do.call(rbind, .)

  if ( "PlateScale_Scalar_S2" %in% names(dat) ) {
    hybnorms[["Plt SF"]] <- tapply(dat$PlateScale_Scalar_S2, dat$PlateId, function(x) x[1]) %>% round(2)
  } else if ( "PlateScale_Scalar_S2"%in%names(atts <- attributes(dat)$Header.Meta$HEADER) )
    hybnorms[["Plt_SF"]] <- atts[ names(atts) ] %>% unlist %>% round(2)

  mednorms <- lapply(spldata, function(.x) {
                       med <- range(.x[, grep("_S2$", getNormNames(.x), value = TRUE)])
                       med <- sprintf("(%0.2f, %0.2f)", med[1], med[2])
                       data.frame(b = med) %>% purrr::set_names("Med SF")
    }) %>% do.call(rbind, .)

   fus <- lapply(spldata, function(.x) {
     med <- range(.x[, grep("ANMLFractionUsed.*_S2$", names(.x), value = TRUE)], na.rm = TRUE)
     med <- sprintf("(%0.2f, %0.2f)", med[1], med[2])
     data.frame(b = med) %>% purrr::set_names("ANMLFU")
   }) %>% do.call(rbind, .)

  cbind(hybnorms, mednorms, fus)

}

