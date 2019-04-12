
#' Create a reference from un-median normalized (but calibrated) data
#' using single-sample adaptive maximum likelihood normalization (ssANML)
#' We start with an initiaReference and an adat (in particular, we use
#' Covance-Boise for the initialReference, and all of Covance ), then use ssANML
#' on all the samples and rebuild references from them. Rinse, repeat until SFs don't change
#'
#' @param adat the data to use for reference building
#' @param initialRef a reference to kick-start the process.
#' If NULL (default), use all of the adat samples
#' @param cutoff the limit to use in the call to ssANML (default: 2), to decide which analytes get dropped
#' @return a reference object (see generateRefParams below) of the final normalized data
generateSingleSampleReference <- function(adat, initialRef=NULL,
                                          effect_cut=2, recenter = FALSE,
                                          maxrelerr=1e-2, maxiter=20) {

  if (is.null(initialRef)) {
    initialRef <- computeRefParams(adat)
  }
  snames <- getAptamers(adat)

  # loop: ssANML, createRef; stop when ref params do no change much
  niter <- 1
  relerr <- 1
  ##
  new_ref <- new_adat <- list()
  ##
  while( (relerr >= maxrelerr) & (niter <= maxiter) ) {
    new_adat[[niter]] <- singleSampleANML(adat, pop_ref = initialRef, effect_cut = effect_cut, do_round = FALSE)$MedDat
    if (recenter) {
      # rescale the adat to keep it at the median of the initialRef median?
      medSF <- apply(new_adat[[niter]][, getNormNames(new_adat[[niter]])], 2, median)
      # apply to each dilution group separately, so as to make median of SFs in each to be 1
      aptDils <- getAptamerDilution(new_adat[[niter]])
      for (dgn in names(aptDils)) {
        if (dgn!="0") {
          normName <- paste0("NormScale.", dgn)
          new_adat[[niter]][,c(normName, aptDils[[dgn]])] %<>% divide_by(medSF[[normName]])
        }
      }
      # # alternative rescaling
      # sf <- median(initialRef$RefMedian)/median(as.matrix(new_adat[[niter]][,snames]))
      # new_adat[[niter]][,c(getNormNames(new_adat[[niter]]),snames)] %>% multiply_by(sf)
    }
    new_ref[[niter]]  <- computeRefParams_kCentral(new_adat[[niter]],
                                                   baseref = initialRef,
                                                   effect_cut = effect_cut)
    # update
    relerr_median <- max((new_ref[[niter]]$RefLog10Median -
                          initialRef$RefLog10Median) / initialRef$RefLog10Median)
    relerr_mad    <- max((new_ref[[niter]]$RefLog10MAD -
                          initialRef$RefLog10MAD) / initialRef$RefLog10MAD)
    relerr <- max(relerr_median, relerr_mad)
    initialRef <- new_ref[[niter]]
    adat  <- new_adat[[niter]]
    niter <- niter + 1
    # report progress
    cat(sprintf("%03d: relerr_median = %5.3f, relerr_mad = %5.3f, relerr = %5.3f\n",
                niter-1, relerr_median, relerr_mad, relerr))
    cat(apply(adat[,getNormNames(adat)],2,range)[1,], "\n")
    cat(apply(adat[,getNormNames(adat)],2,range)[2,], "\n")
  }
  if ((relerr >= maxrelerr)) {
    warning("Reference has not converged after %d iterations", maxiter)
  } else {
    cat(sprintf("Converged after  %d iterations, relerr = %s", niter-1, relerr))
  }
  invisible(list(new_ref=new_ref, final_adat=new_adat))
}

#' Auxiliary function to create references from an adat
#' The references parameters to return are:
#' Univariate: mean, median, sd and mad of RFU values, and same of logRFU values
#' Multivariate: an estimate of the Covariance matrix for RFU and logRFU values
#' @param adat the data from which to create the reference
#' @return a ref data.frame, with columns SeqId, mean, median, RefSD, mad, log_mean, RefLog10Median, log_sd, RefLog10MAD
#'
#' TODO:
#' # a list with names RFU and logRFU, each element consisting of a list of
#' # $uni: a dataframe of parameters with one SOMAmer per row and columns:
#' # median, mad (for logRFU), median sd (for RFU; since it is not known how to translare log-mad to mad)
#' $multi: list of
#'   $CovM a matrix, estimate of the covariance matrix
#'   $PrecM, estimate of the covariance matrix
computeRefParams <- function(adat) {
  snames <- getAptamers(adat)
  sdata <- getAptData(adat)
  log_adat <- log10(adat)

  # Compute from all the adat samples, as given
  newref <- data.frame(
                        Target  = sdata$Target,
                        SeqId = getSeqId(snames, trim.version = TRUE),
                        Type    = sdata$Type,
                        Dilution= sdata$Dilution,
                        RefLog10Median = apply(log_adat[,snames], 2, median, na.rm=TRUE),
                        RefLog10MAD = apply(log_adat[,snames], 2, mad, na.rm=TRUE, constant=1.4826),
                        RefMedian = apply(adat[,snames], 2, median, na.rm=TRUE),
                        RefSD = apply(adat[,snames], 2, sd, na.rm=TRUE)
    )
  newref
}

#' Auxiliary function to create references from an adat
#' The references parameters to return are:
#' Univariate: mean, median, sd and mad of RFU values, and same of logRFU values
#' Multivariate: an estimate of the Covariance matrix for RFU and logRFU values
#' @param adat the data from which to create the reference
#' @param baseref the basic reference (median and mad) to select samples to use for each analyte
#' @param effect_cut cutoff for considering values to estimate params (defaut=2)
#' @return a ref data.frame, with columns SeqId, mean, median, RefSD, mad, log_mean, RefLog10Median, log_sd, RefLog10MAD
#'
#' TODO:
#' # a list with names RFU and logRFU, each element consisting of a list of
#' # $uni: a dataframe of parameters with one SOMAmer per row and columns:
#' # median, mad (for logRFU), median sd (for RFU; since it is not known how to translare log-mad to mad)
#' $multi: list of
#'   $CovM a matrix, estimate of the covariance matrix
#'   $PrecM, estimate of the covariance matrix
computeRefParams_kCentral <- function(adat, baseref=NULL, effect_cut=2) {
  snames <- getAptamers(adat)
  sdata <- getAptData(adat)
  log_adat <- log10(adat)

  # if no baseref was passed, compute from all the adat samples, as given
  if(is.null(baseref)) {
    baseref <- computeRefParams(adat)
  }
  # set output df, copying input one
  newref <- baseref
  # assume ref, snames are of the same SOMAmers, in the same order
  effect <- abs(t((t(log_adat[,snames]) - baseref$RefLog10Median)/baseref$RefLog10MAD))
  samp_use <- !is.na(effect) & ( effect <= effect_cut )
  # now use these samples to estimate the params (k-central normal dist style)
  newref$RefLog10Median <- sapply(1:ncol(samp_use), function(j)
    median(log_adat[samp_use[,j],snames[j]], na.rm = TRUE))
  newref$RefLog10MAD <- sapply(1:ncol(samp_use), function(j)
    kCentralMAD(log_adat[samp_use[,j],snames[j]]))
  # estimate the RFU parameters based on the values from the logRFU distributions, since we can't use k-Central estimates
  log_mean <- sapply(1:ncol(samp_use), function(j)
    mean(log_adat[samp_use[,j],snames[j]], na.rm = TRUE))
  log_sd   <- sapply(1:ncol(samp_use), function(j)
    kCentralSD(log_adat[samp_use[,j],snames[j]]))
  # the median of a logNormal is the exp(mean)
  newref$RefMedian <- 10^log_mean
  # the CV of a logNormal is = sqrt(exp(log_sd^2) - 1) and its mean is exp(log_mean + log_sd^2/2)
  cvRFU <- sqrt(10^(log(10)*log_sd^2) - 1)
  meanRFU <- 10^(log_mean + log(10)/2 * log_sd^2)
  # hence, the sd is...
  newref$RefSD <- cvRFU * meanRFU
  # just in case baseref had more SOMAmers than the adat:
  newref[newref$SeqId %in% getSeqId(snames, trim.version = TRUE),]
}

# other auxiliary functions
kCentralSD <- function(x, k=2) {
  max( abs(x-mean(x, na.rm = TRUE))/k, sd(x, na.rm = TRUE))
}
kCentralMAD <- function(x, k=2) {
  max( abs(x-median(x, na.rm = TRUE))/k, mad(x, na.rm = TRUE, constant = 1.4826))
}
