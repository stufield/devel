
#' Robust Linear Model Calibration
#'
#' Performs calibration using robust linear model fitting to account for
#' scale and bias shifts in RFU between runs.
#'
#' @param data1 A data frame of class `soma_adat` containing RFU values from
#' a previous run.
#' @param data2 A data frame of class `soma_adat` containing RFU values from
#' a new assay run with RFU values to be calibrated using robust linear calibration.
#' @param apts Which features (columns) in the objects to calibrate. Can be a
#' subset of the total features. If `apts = NULL` (default), the intersect of both
#' data frames is used and each feature is calibrated.
#' @param data1.id Column name used as row identifiers to order and sync the data frames.
#' @param data2.id Column name used as row identifiers to order and sync the data frames.
#' By default, the same string is used for both data frames.
#' @param verbose Logical. Print out diagnostic information as function runs?
#' @return A calibrated data frame `soma_adat` object containing all meta data and the
#' intersect of the feature data between the two data objects.
#' @author Stu Field
#' @seealso \code{\link[SomaPlyr]{getAptamers}}, \code{\link[SomaPlyr]{getMeta}}
#' @examples
#'
#' @importFrom rlang signal
#' @export rlm_calibration
rlm_calibration <- function(data1, data2, apts = NULL, data1.id,
                            data2.id = data1.id, verbose = TRUE) {

  if ( missing(data1.id) ) {
    rlang::signal("Must provide `data1.id =` column name to match sample IDs.",
                  "error")
  }

  if ( !data1.id %in% names(data1) ) {
    rlang::signal(paste("Column name not found in `data1`:", data1.id),
                  "error")
  }

  if ( !data2.id %in% names(data2) ) {
    rlang::signal(paste("Column name not found in `data2`:", data2.id),
                  "error")
  }

  # reduce the datasets to matching ids and align them; just for fitting the linear model
  fit.data1 <- data1[ which(data1[[data1.id]] %in% data2[[data2.id]]), ]
  fit.data2 <- data2[ which(data2[[data2.id]] %in% data1[[data1.id]]), ]
  fit.data1 <- fit.data1[ order(fit.data1[[data1.id]]), ]
  fit.data2 <- fit.data2[ order(fit.data2[[data2.id]]), ]

  if ( is.null(apts) ) {
    apts <- intersect(getAptamers(fit.data1), getAptamers(fit.data2))
  }

  data.catch(fit.data1, fit.data2, apts = apts, id1 = data1.id, id2 = data2.id)

  if ( median(fit.data2[[apts[1]]], na.rm = TRUE) > 10 || median(fit.data1[[apts[1]]], na.rm = TRUE) > 10 ) {
    do.log <- TRUE
    if ( verbose ) {
      rlang::signal(
        "Values converted to log-space for model fitting and back to linear space",
        "warning")
    }
  } else {
    do.log <- FALSE
  }

  stats <- purrr::map(apts, function(apt) {
      model <- fit.rlm(fit.data1[[apt]], fit.data2[[apt]],
                       do.log = do.log)
      betavec <- coef(model)
      data.frame(beta0 = betavec[1],
                 beta1 = betavec[2],
                 converged = model$converged)
    }) %>% 
    purrr::set_names(apts)

  stats   <- do.call("rbind", stats)
  no.conv <- rownames(stats)[!stats$converged]

  if ( length(no.conv) > 0 && verbose ) {
    writeLines("*  Warning! Convergence failure:")
    sapply(no.conv, function(r) cat(sprintf("     %s\n",r)))
  }

  # T(y_2) ~ A + B*y_2
  # A = -(beta0/beta1)
  # B = 1/beta1
  for ( apt in apts ) {
    b0 <- stats[ apt,"beta0" ]
    b1 <- stats[ apt,"beta1" ]
    A <- -(b0/b1)
    B <- (1/b1)
    if ( do.log ) {
      log.rfu <- A + B * log10(data2[[apt]])   # transform in log-space
      data2[[apt]] <- 10^log.rfu               # go back to linear space
    } else {
      data2[[apt]] <- A + B * data2[[apt]]
    }
  }
  return(data2[, c(getMeta(data2), apts)])
}






#' Data Catch
#'
#' A simple trap catch used internally to \code{\link{rlm_calibration}}
#' that is meant to catch any data mismatches that would cause the model
#' fitting to fail.
#'
#' @param d1 data frame of RFU data to fit, post sorting and reordering so
#' that the rows match up between d1 and d2.
#' @param d2 data frame of RFU data to fit, post sorting and reordering so
#' that the rows match up between d1 and d2.
#' @param ... Additional arguments passed into the parent function, namely,
#' apts, id1, and id2.
#' @return No return. But if problems are found an error is triggered.
#' @author Stu Field and Kirk DeLisle
#' @noRd
data.catch <- function(d1, d2, ...) {

  dots <- list(...)
  apts <- dots$apts
  id1  <- dots$id1
  id2  <- dots$id2

  if ( any(d1[[id1]] != d2[[id2]]) ) {
    rlang::signal(
      "Mismatch in bridging samples. The row ordering may have failed.",
      "error")
  }

  if ( dim(d1)[1] == 0 || dim(d2)[1] == 0 ) {
    rlang::signal("Row matching failure. No matching rows found.", "error")
  }

  if ( any(!apts%in%names(d1)) ) {
    ss <- setdiff(apts, names(d1))
    rlang::signal(
      paste("Names mismatch between `apts` and names of `data1`: ",
            paste(ss, collapse = ", ")), "error")
  }

  if ( any(!apts %in% names(d2)) ) {
    ss <- setdiff(apts, names(d2))
    rlang::signal(
      paste("Names mismatch between `apts` and names of `data2`: ",
            paste(ss, collapse = ", ")), "error")
  }
}





#' Fit RLM model
#'
#' Performs the actual model fitting of the robust linear model
#'
#' @param x original vector of RFU values
#' @param y new vector of RFU values
#' @param do.log Logical. Should numeric values be log-transformed prior to fitting?
#' @return return_value
#' @author Stu Field
#' @seealso \code{\link[MASS]{rlm}}
#' @importFrom MASS rlm
#' @noRd
fit.rlm <- function(x, y, do.log) {
  if ( do.log ) {
    y %<>% log10()
    x %<>% log10()
  }
  suppressWarnings(MASS::rlm(y ~ x, init = "lts", maxit = 100))
}


