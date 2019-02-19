#' Explore the effect of number of analytes on model error
#'
#' Trains models on the data with increasing number of analytes, tests error metrics
#'
#' @param adat The data to use for training/validation (and possible test).
#'   By default, splits in 80/20, \code{n_splits} times.
#' @param method The (\code{caret}) method name for the model (lm, logistic...)
#' @param response A string with the variable name (in \code{adat}) giving the response
#'   \code{adat[[response]]} should be a binary factor (discrete response) or numeric (continuous response)
#'   If a factor, the first level will be considered the case, the second the control.
#' @param preprocess Vector of preprocessing steps. Default c("log10", "center", "scale")
#' @param order Indicates how to order the analytes as they are added to the model
#'   If NULL, the order in \code{somamers} is used.
#'   The default ("response"), is KS-distance for binary responses and
#'   pearson correlation (with response) for continuous ones.
#'   One can also specify "KS" or "t.test" for binary responses or "pearson" or "spearman" for continuous ones,
#'   or give a function that will reorder the SOMAmers as desired. The function should have signature
#'   function(somamers, adat, response_name)
#'   (a vector of somamer names, the adat (contains at least somamers and response_namne), and the name of
#'   the response variable), and return an ordered list of the somamers.
#'   It is your responsibility to check that the function makes sense for the type of response.
#' @param somamers Indicates what SOMAmer to be used.
#'   If an integer N, the top N SOMAmers are used (as ordered by order). Default: 200.
#'   If NULL, all the SOMAmers in \code{adat}, in the order they are returned by getAptamers.
#'   If a character vector, the ordered list of names to be used
#' @param test_set Either an adat with data for an independent test
#'   (to be plotted in addition to the train/val), or
#'   a string \code{"holdout"} indicating to set aside 20% of adat for testing (default), or
#'   NULL to avoid testing altogether
#' @param n_splits The number of 80/20 splits to use
#' @param n_points The number of points to compute and plot (distinct number of analytes to consider). Defaults
#'   to 20 (or less if maximum number of analytes is less than 20)
#' @param plot_title an optional string with the title for the plots (defaults to "Errors vs. model complexity")
#' @param ... options to pass on to caret (like train.control)
#' @return A list of
#' * soma_list: The ordered list of the SOMAmers used.
#' * metrics: A list of train/validation/test metrics. First level is the metric,
#'    the second one the data set (train, val, test) and each of these is a matrix
#'    (with n_split rows and N-1 (N: max # analytes) columns (starts at 2 analytes))
#' * median_metrics: Medians of \code{metrics} over the \code{n_splits}
#'    (list of metrics, each a matrix with N-1 rows and 2 or 3 columns (train, val, possibly test)
#' * all_plot: a ggplot of the error metrics as function of the number of analytes, for all the 80/20 splits
#' * median_plot: a ggplot of the median of the error metrics, across the 80/20 splits
#' @examples
#' # Look at the Lean Body Mass lm models, using the 'top' SOMAmers up to 1000:
#' exp_lbm1000 <- exploreNAnalytes(fen_data,
#'                                 method = "lm", response = "CLIN_DexaTotalLeanMass_.IE1",
#'                                 preprocess = c("center", "scale"),
#'                                 somamers = 1000,
#'                                 plot_title = "Lean body mass", n_points = 20)
#' exp_lbm1000$med_plot
#' # Or the 115 used in the final model (in the given order):
#' soma_final <- read.csv("leanbodymass_model_final_features.csv")
#' exp_lbm_final <- exploreNAnalytes(fen_data,
#'                                    method = "lm", response = "CLIN_DexaTotalLeanMass_.IE1",
#'                                    preprocess = c("center", "scale"),
#'                                    somamers = soma_final$aptName,
#'                                    order = NULL,
#'                                    plot_title = "Lean body mass", n_points = 20)
#' exp_lbm_final$med_plot
#' @export exploreNAnalytes
#'
exploreNAnalytes <- function(
    adat, method, response,
    preprocess = c("log10", "center", "scale"),
    order = "response",
    somamers = 200,
    test_set = "holdout",
    n_splits = 10,
    n_points = 20,
    plot_title = "Errors vs. model complexity",
    ...) {

  ## needed libraries
  library(caret)
  library(pROC)

  ## some input checks
  continuous <- TRUE
  if (!(response %in% names(adat))) {
    stop(sprintf("Response %s should be a variable in adat", response))
  } else if (!is.numeric(adat[[response]])) {
    # try to force into a binary factor
    adat[[response]] <- factor(adat[[response]])
    if (length(levels(adat[[response]])) != 2) {
      stop(sprintf("Discrete response %s should be binary (factor with two levels)", response))
    } else {
      continuous = FALSE
    }
  } # otherwise, assume continuous response
  # check order value
  if (!is.null(order) && !("function" %in% class(order))) {
    order <- str_to_lower(order)
    if (!(order %in% c("response", "ks", "t.test", "pearson", "spearman")))
      stop("order should be either a function or one of 'response', 'ks', 't.test', 'pearson', 'spearman'")
  }
  #
  if (is.null(plot_title)) plot_title <- ""
  # # expand ... to add trainControl(method="none") to arguments passed to caret's train
  # args <- list(...)
  # if (!is.null(args$trControl)) {
  #   # unfortunately, there is no other way to know that an existing "method" is not put there
  #   # by default trainControl(), other than asking for a flag
  #   if (is.null(args$trControl$forceMethod) || !args$trControl$forceMethod) args$trControl$method <- "none"
  # } else {
  #   args$trControl <- list(method="none")
  # }

  ## compute preprocess for all data
  snames <- getAptamers(adat)
  if ("log10" %in% preprocess) {
    adat <- log10(adat)
  }
  ad_prePro <- caret::preProcess(adat[,snames], method=setdiff(preprocess, "log10"))
  adat <- predict(ad_prePro, adat)

  ## restrict SOMAmers in adat, if needed
  # get the list, first
  if (is.null(somamers)) {
    somamers <- snames
    N <- length(snames)
  } else if (is.numeric(somamers)) {
    N <- somamers
    somamers <- snames[1:N]
  } else if (is.character(somamers)) {
    N <- length(somamers)
    # check that all names are in adat
    if (!all(somamers %in% snames)) {
      stop("The elements of somamers should all be names in adat")
    }
  }
  # reorder, if necessary
  if (!is.null(order)) {
    if (class(order) == "character") {
      somamers <- order_somamers(somamers, adat, response, method = order)
    } else if ("function" %in% class(order)) {
      somamers <- order(somamers, adat, response)
    }
  }
  # now restrict
  adat <- adat[,c(somamers, response)]

  ## set up test adat, if needed
  ads  <- list()
  if (!is.null(test_set)) {
    if (is.character(test_set) && test_set=="holdout") {
      test_ix <- caret::createDataPartition(adat[[response]], p=0.2)[[1]]
      ads$test <- adat[test_ix, c(somamers, response)]
      adat    <- adat[-test_ix, c(somamers, response)]
    } else if ("soma.adat" %in% class(test_set)) {
      if ("log10" %in% preprocess) {
        test_sn <- getAptamers(test_sn)
        test_set[,test_sn] <- log10(test_set[,test_sn])
      }
      ads$test  <- predict(ad_prePro, test_set)[ , c(somamers, response)]
    }
  }

  ## create the 80/20 splits
  train_ix <- caret::createDataPartition(adat[[response]], p = 0.8, times = n_splits)

  ## train for n_points SOMAmers, equally spaced from j=2 to N, then compute errors
  j_pts <- unique(floor(seq(2, N, length.out = n_points)))
  ads$train <- ads$val <- NA
  # set up error lists according to response
  cont_errs <- c("MAE", "RMSE")
  disc_errs <- c("Sensitivity", "Specificity", "AUC", "BalancedAccuracy", "probMAE")
  errs <- list()
  if (continuous) {
    errns <- cont_errs
  } else {
    errns <- disc_errs
  }
  for (errn in errns) errs[[errn]] <- list()
  # initialize to matrix of N cols and n_splits rows
  for (adn in names(ads)) {
    for (errn in names(errs)) {
      errs[[errn]][[adn]] <- matrix(NA, nrow = n_splits, ncol = length(j_pts)) %>% set_colnames(j_pts)
    }
  }

  pb <- txtProgressBar(style=3)
  for (i in 1:n_splits) {
    # set up train, val (test) sets ?
    ads$train <- adat[train_ix[[i]], ]
    ads$val  <- adat[-train_ix[[i]], ]
    for (j in seq(j_pts)) {
      # train (start from 2 analytes)
      fit <-
        caret::train(x = ads$train[,somamers[1:j_pts[[j]]], drop=FALSE],
                     y = ads$train[[response]],
                     method = method,
                     trControl=trainControl(method = "none"),
                     ...)
      # test on all sets: train, validation, test (if any)
      if (continuous) {
        preds <- lapply(ads, function(ad) predict(fit, ad))
      } else {
        preds <- lapply(ads, function(ad) predict(fit, ad, type="prob"))
        rocs  <- lapply(names(ads), function(adn) pROC::roc(ads[[adn]][[response]], preds[[adn]][,1])) %>%
          set_names(names(ads))
        cutoff <- pROC::coords(rocs$train, x="best", best.method="closest.topleft")["threshold"]
      }
      # compute errors (continuous for now)
      for (adn in names(preds)) {
        for (errn in names(errs)) {
          errs[[errn]][[adn]][i,as.character(j_pts[[j]])] <-
            switch(errn,
                   "MAE" = mean(abs(ads[[adn]][[response]] - preds[[adn]])),
                   "RMSE"= sqrt(mean((ads[[adn]][[response]] - preds[[adn]])^2)),
                   "AUC" = as.numeric(rocs[[adn]]$auc),
                   "Sensitivity" = pROC::coords(rocs[[adn]], x=cutoff)["sensitivity"],
                   "Specificity" = pROC::coords(rocs[[adn]], x=cutoff)["specificity"],
                   "BalancedAccuracy" = (pROC::coords(rocs[[adn]], x=cutoff)["sensitivity"] +
                                         pROC::coords(rocs[[adn]], x=cutoff)["specificity"])/2,
                   "probMAE" = mean(abs(
                     (as.numeric(ads[[adn]][[response]]) %% 2) - preds[[adn]][,1]))
                   )
        }
      }
      setTxtProgressBar(pb, ((i-1) * length(j_pts) + j) / (length(j_pts) * n_splits))
    }
  }

  # compute median errors over all 80/20 splits
  med_errs <- lapply(errs, function(err) sapply(err, function(errm) apply(errm, 2, median)))

  ## produce plots
  # all splits together
  tmp <- melt(errs) %>% set_names(c("n_split", "n_analytes", "value", "data", "metric"))
  tmp$data <- factor(tmp$data, levels=union(c("train", "val"), unique(tmp$data)))
  all_splits_plot <-
    ggplot(tmp, aes(x=n_analytes, y=value, color=data, group=paste(n_split,data,sep = "_"))) +
    facet_wrap(~metric, ncol = 3, scales = "free_y") +
    geom_point(alpha = 0.6) +
    stat_smooth(geom = 'line', size = 1, alpha = 0.3, se = FALSE) +
    labs (x= "Number of analytes", y = "error",
          title = plot_title,
          subtitle = sprintf("model: %s, response: %s", method, response))
  # median values only
  med_tmp <- melt(med_errs) %>% set_names(c("n_analytes", "data", "value", "metric"))
  med_tmp$data <- factor(med_tmp$data, levels=union(c("train", "val"), unique(med_tmp$data)))
  medians_plot <-
    ggplot(med_tmp, aes(x=n_analytes, y=value, color=data)) +
    facet_wrap(~metric, ncol = 3, scales = "free_y") +
    geom_point(alpha = 0.6) +
    geom_smooth() +
    labs (x= "Number of analytes", y = "error",
          title = plot_title,
          subtitle = sprintf("model: %s, response: %s", method, response))

  ## return data and plots
  invisible(
    list(soma_list = somamers,
       metrics = errs, median_metrics = med_errs,
       all_plot = all_splits_plot, median_plot = medians_plot)
  )

}

#' Auxiliary function to order SOMAmers, according to a response
#'
#' @param soma_list a vector of SOMAmer names
#' @param adat  an adat with SOMAmer values (and response)
#' @param response the names of the response variable in adat (either a binary factor or continuous)
#' @param method an (optional) method to use for ordering. Defaults to 'KS' for factor responses and 'pearson'
#'   for continuous ones; it could also be t.test (factor) or spearman (continuous)
#'
#' @return a list of somamer names, same as soma_list, but ordered according to 'method'
order_somamers <- function(soma_list, adat, response, method="response") {

  if (is.factor(adat[[response]])) {
    # check it is binary
    if (length(levels(adat[[response]])) != 2) {
      stop(sprintf("A factor response should be binary (2 levels.) Found %s instead.",
                   length(levels(adat[[response]]))))
    }
    tmp_l <- split(adat, adat[[response]])
    tmp_D <-
      switch(method,
           "response" = ,
           "KS" =
              sapply(soma_list, function(sn) as.numeric(ks.test(tmp_l[[1]][,sn],
                                                               tmp_l[[2]][,sn])$statistic)),
           "t.test" =
             abs(
               sapply(soma_list, function(sn) as.numeric(t.test(tmp_l[[1]][,sn],
                                                                tmp_l[[2]][,sn])$statistic))
             ),
           stop(sprintf("binary response requires KS or t.test ordering, cannot use %s", method))
    )
  } else if (is.numeric(adat[[response]])) {
    # use correlation
    tmp_D <-
      switch(method,
             "response" = ,
             "pearson" =
               sapply(soma_list, function(sn) cor(adat[,sn], adat[[response]], method = "pearson")),
             "spearman" =
               sapply(soma_list, function(sn) cor(adat[,sn], adat[[response]], method = "spearman")),
             stop(sprintf("continuous response requires pearson or spearman ordering, cannot use %s", method))
      )
   } else {
    # bail out
    stop(sprintf("The response should be a binary factor or continuous, not %s.",
                 class(adat[[response]])))
  }

  names(sort(abs(tmp_D), decreasing = TRUE))
}

