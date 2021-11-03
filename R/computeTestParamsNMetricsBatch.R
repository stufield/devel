#' Compute Test Metrics by Batch
#'
#' Given a list of m X of metrics distributions, to be tested at once, compute acceptance limits and estimate 
#' effect sizes and probablity of running into (production) limits
#' We can have n test batches (Serum & Plasma, or several repetitions of same test) and we will 
#' pass if all of them pass (so, no test-retest situation here)
#' Here we only treat one batch at a time; if the X distributions differ (ie: plasma & serum) we would need to run
#' them seprately. But we do indicate the total number of batches, so we can adjust for the desired final Type I error p.
#' We'll assume that per-test Type I error rates and power are equally divided between all metrics, so that the 
#' per-metric/batch Type I error is p1 = 1 - (1-p)^(1/(n*m)) 
#' Note that all list arguments should have names matching X
#'
#' @param X is a list of 'distributions' (ie: just a vector of values representing samples from the distribution) of the values
#' to be tested. One per metric (for instance: one for average QC ratio tails, one for average median Total CV, etc...)
#' @param prod_limit a list of values at which production runs start to be considered failed. It relates to Xone
#' @param Xone the current (recent) distribution of the statistic over which we are applying production acceptance limits
#' X is the disitrbution of the average of Xones, usually. Xone could be the same as X if there is only one measurement in the metric
#' @param nbatch the number of test batches we are planning to run. Could be 1, could be 2 (plasma and serum, for instance) or 
#' whatever ... here only so that the p and betas are adjusted properly. If the tests in different batches differ in 
#' distributons or some other way (think plasma and serum) then you'd need to run this function twice to get the correct 
#' acceptance limits for each
#' @param typeI the desired *final* typeI error rate (0.01, by default)
#' @param power the desired *final* power to test (will determine how far we are likely to detect changes) (0.99, by default)
#' @param prod_limit_fraction what level of failures we want to use for the TypeII computations (0.05, by default)
#' A single number, to be used for all metrics
#' @param alternative how do we model the alterenative hypothesis? either "scaled" (multiplicative effect) or 
#' "shifted" (additive effect)
#' @return a list (class test_params) with several per-metric/batch values:
#' \item{accept_limit}{the cutoff values for the experiment}
#' \item{prob_bad}{a probability value (a scalar) for at least one of the metrics to eventually reach production limits 
#' at least prod_limit_fraction of the times.}
#' \item{min_shift}{the minimum shifts we can expect to detect, with (overall) probability power
#' ie: if the shift is less than this, the probability of rejecting at least one of them is at least power
#' \item{min_shift_bad}{the minimum shifts we can accept until we see production rejections}
#' of about prod_limit_fraction of the runs (this depends on Xone and prod_limit and prod_limit_fraction, 
#' not acceptance_limit)}
#' \item{min_n_bad}{the number of times a test like this should produce typeII errors of size min_shift until we reach 
#' production rejection troubles: (log(min_shift)/log(accept_limit) for scaled)}
#' \item{prob_n_bad}{probability of min_n_bad happening (1-power)^(min_n_bad)}
#' \item{typeI1}{per-metric/batch typeI error (so that in the end we have the given typeI)}
#' \item{beta1}{per-mmetric/batch power (so that in the end we have the given power)}
#' @note that one can use 'summary' on the return list to obtain a table of all 
#' the accept_limit, min_shift and prob_n_bad and the prob_bad values in a nicer format.
#' @examples
#' # join data to send for computation
#' X_ <- Xone_ <- prod_limit_ <- alternative_ <- list()
#' # use a for loop to set first by matrix (plasma and serum), then metric
#' for (mn in names(V4U_20190730)) {
#'   ## QCR
#'   X_[[mn]]$QCT <- V4U_qct_3runAvg_20190730[[mn]]
#'   Xone_[[mn]]$QCT <- V4U_qct_20190730[[mn]]
#'   prod_limit_[[mn]]$QCT <- 0.15
#'   alternative_[[mn]]$QCT <- "scaled"
#'   ## CVM
#'   X_[[mn]]$CVM <- V4U_cvt_median_3LotAvg_20190730[[mn]]
#'   Xone_[[mn]]$CVM <- V4U_cvt_median_20190730[[mn]]
#'   prod_limit_[[mn]]$CVM <- 0.075
#'   alternative_[[mn]]$CVM <- "scaled"
#'   ## CVT
#'   X_[[mn]]$CVT <- V4U_cvt_tail_3LotAvg_20190730[[mn]]
#'   Xone_[[mn]]$CVT <- V4U_cvt_tail_20190730[[mn]]
#'   prod_limit_[[mn]]$CVT <- 0.15
#'   alternative_[[mn]]$CVT <- "scaled"
#' }
#' ## all metrics together, for 3QC tails
#' all_metrics_3QC <- lapply(names(X_), function(mn)
#'   computeTestParamsNMetricsBatch(
#'     X = X_[[mn]], prod_limit = prod_limit_[[mn]], 
#'     Xone = Xone_[[mn]], 
#'     nbatch = 2,
#'     typeI = 0.01, power = 0.99,
#'     prod_limit_fraction = 0.05, 
#'     alternative = alternative_[[mn]]
#'   )) %>% set_names(names(X_)) 
#' #
#' summary_metrics_3QC <- lapply(all_metrics_3QC, summary)
#' @export
computeTestParamsNMetricsBatch <- function(X, prod_limit, 
                                           Xone = NULL, nbatch=1, 
                                           typeI=0.01, power=0.99,
                                           prod_limit_fraction=0.05,
                                           alternative
) {
  # just to avoid errors in calling
  if (!is.list(X) || (length(X) == 2 && names(X)==c("Plasma","Serum")) )
    stop("X must be a list of vectors of values (one vector per metric), for only one matrix.\nPerhaps you meant to call computeTestsParams?\n")
  # if not given, use Xone = X
  if (is.null(Xone)) Xone <- X
  # compute the per-barch/metric error rates and powers desired
  nm <- length(X)*nbatch
  p1 <- 1-(1-typeI)^(1/nm) 
  beta1 <- 1-(1-power)^(1/nm)
  # compute the acceptance limits per each metric
  accept_limit <- min_shift <- min_shift_bad <- 
    prob_n_bad <- min_n_bad <- rep(NA, length=length(X)) %>% set_names(names(X))
  for (mn in names(X)) {
    accept_limit[mn] <- quantile(X[[mn]], prob=1-p1) 
    # how much we can scale or shift up, until prob starts getting below 1-power
    # (ie: we have prob > power of detecting a change larger than this)
    # how is H1 related to H0?
    alt <- match.arg(alternative[[mn]], c("scaled", "shifted"))
    if (alt=="shifted") {
      min_shift[mn] <- accept_limit[mn] - quantile(X[[mn]], prob=1-beta1)
      # how long until we reach production acceptance limits more than 5% (say) of the time?
      min_shift_bad[mn] <- prod_limit[[mn]] - quantile(Xone[[mn]], probs=1-prod_limit_fraction) 
      min_n_bad[mn] <- min_shift_bad[mn] / min_shift[mn]
    } else {
      min_shift[mn] <- accept_limit[mn] / quantile(X[[mn]], prob=1-beta1)
      # how long until we reach production acceptance limits more than 5% (say) of the time?
      min_shift_bad[mn] <- prod_limit[[mn]] / quantile(Xone[[mn]], probs=1-prod_limit_fraction)
      min_n_bad[mn] <- log(min_shift_bad[mn]) / log(min_shift[mn])
    }
    prob_n_bad[mn] <- (1-power)^(min_n_bad[mn])
  }
  # overall chance of running into a production limit: if we are not running into a limit it means none of them did
  prob_bad <- 1-prod(1-prob_n_bad)
  out <- list(accept_limit = accept_limit,
              min_shift = min_shift,
              prob_bad = prob_bad,
              min_shift_bad = min_shift_bad,
              min_n_bad = min_n_bad,
              prob_n_bad = prob_n_bad,
              typeI1 = p1,
              beta1 = beta1) 
  invisible(addClass(out, "test_params"))
}

# summarize (in a nice format) results of test_params 
# acceptance_limit, min_shift, prob_nbad in a matrix; prob_bad by itself

summary.test_params <- function(testParams) {
  out <- list()
  out$perMetric <- cbind(signif(testParams$accept_limit,2), signif(testParams$min_shift,3), signif(testParams$prob_n_bad,1)) %>% 
    set_colnames(c("accept_limit","min_shift","prob_prod_lim"))
  out$prob_bad <- signif(testParams$prob_bad, 1)
  out
}
