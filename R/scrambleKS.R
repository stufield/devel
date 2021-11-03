#' Scramble KS-distances
#'
#' Class scrambling of the \code{Response} variable and recalculate
#' KS-distances for an empirical bootstrap of the likelihood of acheiving same
#' KS scores. Bars represent 95\% confidence interval about the unscrambled
#' KS-distance for each aptamer.
#'
#' @param data A data frame containing SOMAmer data and a "Response" column, preferably
#' a "training.data" object, see \code{\link{createTrainingData}}.
#' @param apts A vector character string of aptamers to be considered in the analysis.
#' @param response Character. String of the column name to be scrambled ... defaults
#' to the \code{Response} column of the data frame (ideal for "training.data" objects).
#' @param nperm Integer. The number of bootstrap trials to perform when calculating
#' the confidence intervals (95\%).
#' @param do.plot Should a plot be created of the scrambled data?
#' @param random.seed Numeric. Optional random seed to set the permutations for
#' reproducible scrambling.
#' @param x Data object of class "scramble.ks" when using the S3 plot method.
#' @param plot.range Subset range of the top KS values to plot. Defaults to all
#' SOMAmers in the object.
#' @param title Title label for the plot.
#' @param MC Logical. Should multicore processing be used? Linux/Mac only.
#' @param cores Integer. If \code{MC=TRUE}, the number of cores to use.
#' @param ... Additional arguments passed to \code{\link{plot.scramble.ks}}
#' OR additional argumnets passed to \code{\link[graphics]{barplot}} (if called
#' during the \code{plot} method.
#' @return An object of class \code{"scramble.ks"}.
#' @author Stu Field
#' @seealso \code{\link[graphics]{barplot}}, \code{\link{scramble.ks}}
#' @examples
#' trdat <- convert2TrainingData(sample.adat, "SampleGroup")
#' perm <- scrambleKS(trdat, apts=getAptamers(trdat), verb=TRUE, random.seed=101) # 30s
#' perm <- scrambleKS(trdat, apts=getAptamers(trdat), random.seed=101, MC=TRUE)   # 06s
#' @importFrom parallel mclapply
#' @export
scrambleKS <- function(data, apts, response = "Response", nperm = 50,
                       do.plot = FALSE, random.seed = sample(1000, 1),
                       verbose = getOption("verbose"), MC = FALSE, cores = 7, ...) {

  stopifnot(response%in%names(data))
  op <- options(warn = -1)
  on.exit(options(op))
  class1      <- levels(as.factor(data[[response]]))[1]
  class2      <- levels(as.factor(data[[response]]))[2]
  disease_idx <- which(data[[response]] == class2)

  stat_vec <- apply(data[, apts], 2, column_ks, disease_idx)["signed.ks.dist", ] %>%
    jitter(amount = 0.0001) %>%
    sort(abs(.), decreasing = TRUE)
  #print(stat_vec)

  data_mat <- data.matrix(data[, names(stat_vec)])  # reorder for names after apply()

  permFun <- function(.x) {
    if ( verbose ) {
      if ( .x%%10 == 0 ) cat("*") else cat(".")
      if ( .x == nperm ) cat("\n")
    }
    scramble_vec <- scrambleClasses(data, field=response, random.seed)  # scramble
    disease_idx  <- which(scramble_vec == class2)
    apply(data_mat, 2, column_ks, disease_idx)["ks.dist", ] %>%
      jitter(amount = 0.0001) %>% sort(decreasing = TRUE)
  }

  if ( grepl("ming", R.version$os) )   # if Windows, no parallel
    MC <- FALSE

  # columns are the number of trials
  # rows are the features
  if ( MC ) {
    perm <- parallel::mclapply(seq(nperm), permFun,
                               mc.cores = cores, mc.set.seed = TRUE) %>%
      do.call(cbind, .)
  } else {
    perm <- sapply(seq(nperm), permFun)
  }
  #print(head(perm))

  CI95      <- apply(perm, 1, quantile, probs = c(0.025, 0.5, 0.975)) %>% t()
  stat_vec  <- stat_vec[ order(abs(stat_vec), decreasing = TRUE) ]
  pn        <- ifelse(stat_vec > 0, 1, 0)  # pos/neg
  stat_vec %<>% abs()
  perm_data <- cbind(ks = stat_vec, direction = pn, CI95)
  ret       <- list(perm_data = perm_data, perm_mat = perm,
                    nperm = nperm, data = deparse(substitute(data)),
                    call = match.call(expand.dots = TRUE))
  ret %<>% addClass("scramble.ks")

  if ( do.plot ) {
    plot(ret, ...)
  }
  invisible(ret)
}


#' @describeIn scrambleKS
#' Plot Scrambled KS-distances. An S3 plot method for objects of class "scramble.ks".
#'
#' @param plot.range A range (or subset) of features to plot (rather than
#' plotting the entire array.
#' @param plot.title Character. A string for the plot title.
#' @examples
#' # S3 plot method
#' plot(perm, plot.range = 1:15)
#' @method plot scramble.ks
#' @export
plot.scramble.ks <- function(x, plot.range = NULL, plot.title = "KS distances", ...) {

  if ( is.null(plot.range) ) {                       # reduce if lower dimensions to be plotted
    plot.range <- 1:nrow(x$perm_data)
  }

  x$perm_data %<>% .[ plot.range, ]
  xlabs  <- rownames(x$perm_data) %>% removeSeqId()   # x-axis labels
  stat_vec <- x$perm_data[, "ks"]
  upvec  <- x$perm_data[, "97.5%"]
  lovec  <- x$perm_data[, "2.5%"]

  barplot(stat_vec, names = if ( length(xlabs) > 100 ) NULL else xlabs,
          ylim = c(0, max(stat_vec) + 0.2), las = 2,
          col = "gray80", cex.names = 0.75, border = NA, ...)

  y <- barplot(x$perm_data[, "50%"],
               col = ifelse(x$perm_data[, "direction"] == 1,
                            col.string[1], col.string[2]),
               axes = FALSE, axisnames = FALSE, angle = 225,
               border = NA, add = TRUE, density = 20)
  lines(y, x$perm_data[, "50%"], type = "b", col = 1, lty = 1,
        lwd = 1.5, pch = 19, cex = 0.5)
  title(sprintf("%s & empirical CI95%% (nperm=%i)\n Top %i SOMAmers",
                plot.title, x$nperm, length(plot.range)))
  ci_col <- ggplot2::alpha("blue", 0.35)
  legend("topright", legend = c("Actual KS","+/- in Disease","Permuted Median","CI95"),
         pch = c(15, 22, 20, 15), lty = c(NA, NA, 1, NA), cex = 1.1,
         bty = "n", lwd = c(0, 2, 2, 0), bg = NULL, pt.bg = col.string[1],
         col = c("gray80", col.string[2], "black", ci_col))
  left  <- par("usr")[1]
  right <- par("usr")[2]
  addPolygon(list(c(left, y, right), c(upvec[1], upvec, upvec[length(upvec)])),
             list(c(left, y, right), c(lovec[1], lovec, lovec[length(lovec)])),
             col = ci_col, add = TRUE)
}

