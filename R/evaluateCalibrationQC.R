# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
######################

#' Evaluate calibration via QC
#'
#' This function calculates the minimum ratio of the QC samples to their
#' respective reference, or expected value.
#'
#' For each QC-sample on each plate, a ratio of the QC-reference/QC-sample is
#' calculated, for typically 4 samples per plate (duplicate samples x 2 QC
#' lots). The absolute value of the log of this ratio is calculated and the
#' *minimum* of the 4 transformed values is returned. The QC reference can
#' be either calculated internally (default) or passed via the
#' \code{\link{calcQCref}} argument. If calculating externally, the
#' QC-reference must be made from calibrated QC-samples from the same runs used
#' to create the reference to which the current data was calibrated (i.e. for
#' internal calibration, said data is itself).
#'
#' @param cal.data A `soma.adat` object of calibrated data containing QC
#' samples of interest.
#' @param QCref A data.frame object with rows as the SOMAmers and columns as
#' vectors of references for each of the QC lots. As a default, the
#' `QCref` is calculated according to an internal reference. If other
#' references are to be used, see \code{\link{calcQCref}}, or create one
#' yourself.
#' @return A data frame containing the absolute values of the log-transformed
#' ratios of the QC-reference / qc-value for each plate and QC lot. These
#' values are then used to evaluate calibration performance. Median CDFs of
#' each of the runs should be less than 0.1(?) (**refer to SOP**).
#' @author Stu Field
#' @seealso \code{\link{filterAdat}}, \code{\link{calcQCref}}, \code{\link{getAptamers}}
#' @references Darryl Perry
#' @examples
#' \dontrun{
#' evaluateCalibrationQC(calibrated.data)
#' }
#' @importFrom purrr map map_df
#' @importFrom magrittr set_names set_rownames "%<>%" "%>%"
#' @export evaluateCalibrationQC
evaluateCalibrationQC <- function(cal.data, QCref = NULL) {

  apts       <- getAptamers(cal.data)
  plates     <- cal.data$PlateId %>% unique()
  plate_data <- purrr::map(plates, function(.x) {
                           filterAdat(cal.data,
                                      PlateId == .x,
                                      SampleType == "QC") %>%
                              dplyr::arrange(SampleId)
    }) %>%
    magrittr::set_names(stringr::str_replace_all(plates, "[^A-Za-z0-9]", "."))

  if ( is.null(QCref) ) {
    QCref <- filterAdat(cal.data, SampleType == "QC") %>%
      calcQCref()
  }

  if ( !all(rownames(QCref) == apts) ) {
    stop("Something wrong with ordering of somamers in `QCref` vs. `cal.data`",
         call. = FALSE)
  }

  purrr::map(plate_data, function(plate) {
             sapply(seq_len(nrow(plate)), function(.r) {   # calc 4 ratios
                    id  <- plate[.r, "SampleId"]
                    ref <- QCref[, id]
                    (ref / plate[.r, apts]) %>%
                      log() %>%
                      abs() %>%
                      as.numeric()
               }) %>%
             magrittr::set_rownames(apts)
    }) %>%
    purrr::map_df(function(plate)
                  apply(plate, 1, min, na.rm = TRUE))  # choose best
}

