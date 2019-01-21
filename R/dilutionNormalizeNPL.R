# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
######################

get_pars <- function(x) { as.list(x$pars) }


# different flavour of 4PL fit
.n4PLx <- function(pars, data) {
  x <- data$x
  yobs <- data$y
  L <- pars[1]
  U <- pars[2]
  k <- pars[3]
  b <- pars[4]
  yfit <- L + (U - L) / (1 + 10^((k - x) * b))

  if ( !is.null(yobs) ) {
    sum((yobs - yfit)^2)
  } else {
    yfit
  }
}


# 4PL fit function
.n4PL <- function(pars, data) {
  yobs <- data$y
  x <- data$x
  L <- pars[1]
  U <- pars[2]
  k <- pars[3]
  b <- pars[4]
  yfit <- U + ( L - U ) / ( 1 + (x / k)^b )

  if ( !is.null(yobs) ) {
    sum((yobs - yfit)^2)
  } else {
    yfit
  }
}


# 4PL inverse function
.n4PLinv <- function(pars, y) {
  if ( !inherits(pars,"list") ) {
    pars %<>% as.list()
  }
  with(pars, k * ( ((L - U) / (y - U)) - 1 )^(1 / b))
}


# 5PL fit function
.n5PL <- function (pars, data) {
  x <- data$x
  yobs <- data$y
  L <- pars[1]
  U <- pars[2]
  k <- pars[3]
  b <- pars[4]
  s <- pars[5]
  yfit <- L + ((U - L) / (1 + 10^((k - x) * b)))^s

  if ( !is.null(yobs) ) {
    sum((yobs - yfit)^2)
  } else {
    yfit
  }
}




#' Generate Initial Parameter Values
#'
#' Uses the raw data in the to be fit to generate reasonable
#' starting values for the optimization. L is estimated via
#' the 5th percentile, U is estimated via the 95th percentile,
#' k is estimated via the median "x" value, and b is hard coded
#' to 0.5 as typical values of b are in this range. If
#' a 5-parameter model is desired, s = 1 initially.
#'
#' @param data A data frame with 2 columns named "x" and "y"
#' @param npar Numeric. Number of parameters to fit in the logistic model
#' @return Vector of length corresponding to \code{npar} with:
#' L, U, k, b, and possibly s
#' @author Stu Field
#' @seealso \code{\link[stats]{quantile}}, \code{\link[stats]{median}}
#' @noRd
getInit <- function(data, npar = 4) {
  L <- quantile(data$y, 0.05, na.rm = TRUE) %>% unname()
  U <- quantile(data$y, 0.95, na.rm = TRUE) %>% unname()
  k <- median(data$x)
  b <- 0.5
  c(L = L, U = U, k = k, b = b, s = if (npar == 4) NULL else 1)
}



#' Fit an n-Parameter Logistic Model
#'
#' Description ...
#'
#' Currently only supports 4 or 5 parameter models
#'
#' @param data A data frame consisting of 2 columns of data, that *must*
#' be named "x" and "y". In the context of fitting logistic models for dilution
#' normalization, "x" and "y" correspond to the dilutions and RFU values respectively
#' @param npar Numeric. The number of parameters in the logistic model fit.
#' Currently either 4 or 5 are supported, with 4 being the default
#' @param do.log Logical. Should the y-values (RFUs) be log10-transformed
#' proir to fitting?
#' @param ... Additional arguments passed to \code{\link[stats]{optim}}, OR
#' arguments passed to \code{\link{curve}} if used in the S3 plot method, OR
#' arguments passed to the generic S3 \code{print} methods.
#' @return A list containing:
#' \item{data}{The original data for the fitted points (after outlier removal)}
#' \item{minimum}{The minimum value of the objective function}
#' \item{pars}{The 4 (or 5) parameter estimates for: L, U, k, b, and s (if 5PL fit)}
#' \item{iterations}{The number of iterations needed for convergence}
#' \item{convergence}{Was convergence reached, if not, the corresponding error code}
#' \item{fitted.y}{}
#' \item{newx}{}
#' \item{newy}{}
#' \item{residuals}{The residuals of the model fit}
#' \item{pt.infl}{The inflection point of the model}
#' \item{limits}{The linear limits of the logistic curve relative to the RFU axis}
#' \item{stats}{Fitting statistics: Goodness of Fit, Standard Error, p-value, and AIC}
#' \item{npar}{Number of parameters in the logistic fit}
#' \item{outliers}{Were outliers detected and removed prior to the fitting?}
#' @author Stu Field
#' @seealso \code{\link[stats]{optim}}
#' @examples
#' \dontrun{
#' npl <- fit.npl(dilution.data, npar = 4, method = "BFGS", control = list(maxit = 5000))
#' npl
#' npl %>% get_pars
#' npl %>% plot
#' }
#' @importFrom stats optim
#' @importFrom purrr list_modify
#' @importFrom magrittr "%<>%" "%>%"
#' @export fit.npl
fit.npl <- function(data, npar = 4, do.log = TRUE, ...) {

  if ( !setequal(c("x", "y"), names(data)) ) {
    stop("Data frame must contain named x-values and y-values: c('x','y')",
         call. = FALSE)
  }

  .fun <- switch(
    as.character(npar),
    "4" = .n4PL,
    "5" = .n5PL,
    stop("Only 4 or 5 parameter logistic fits currently supported.",
         call. = FALSE)
  )

  if ( do.log ) {
    data$y %<>% log10()
  }

  data  <- data[ order(data$x), ]
  inits <- getInit(data, npar = npar)

  fit      <- stats::optim(inits, .fun, data = data, ...)
  fitted.y <- do.call(.fun, list(pars = fit$par,
                                 data = data.frame(x = data$x)))
  out      <- getOutliers(data$y - fitted.y, n.sigma = 6)

  if ( length(out$idx) > 0 ) {
    cat("* Outliers detected and removed ...", length(out$idx), "\n")
    data <- data[ -out$idx, ]
    fit  <- stats::optim(inits, .fun, data = data, ...)
  }

  pars      <- fit$par
  fitted.y  <- do.call(.fun, list(pars = pars,
                                  data = data.frame(x = data$x)))
  residuals <- data$y - fitted.y

  list(
    data        = data,
    minimum     = fit$value,
    pars        = pars,
    iterations  = fit$counts[1],
    convergence = fit$convergence,
    fitted.y    = fitted.y,
    newx        = seq(min(data$x), max(data$x), length = 100),
    newy        = do.call(.fun, list(pars = pars,
                                     data = data.frame(x = ret$newx))),
    residuals   = residuals,
    pt.infl     = getInflectionPt(pars = pars, .fun = .fun),
    limits      = calc.limits(pars),
    stats       = get.npl.stats(data$y, fitted.y),
    npar        = ifelse(npar == 4, "4PL", "5PL"),
    outliers    = length(out$idx) > 0
    ) %>%
    purrr::list_modify(stats = c(.$stats, AIC = calc.aic(residuals, npar))) %>%
    addClass("npl")
}


#' Calculate Goodness of Fit
#'
#' Computes the Goodness of fit for an n-parameter logistic model
#'
#' @param y Numeric. The y-values of the data
#' @param yfit Numeric. The fitted values based on the n-PL model fit
#' @return The Goodness of fit for the model
#' @noRd
gof <- function(y, yfit) {
   n <- length(y)
   S2y <- var(y)
   w <- (y - yfit)^2
   1 - sum(w^2) / ((n - 1) * S2y)
}


#' @noRd
getInflectionPt <- function(pars, .fun) {
   cbind.data.frame(x=pars["k"],
                    y=do.call(.fun, list(pars=pars, data=data.frame(x=pars["k"]))))
}


#' Calculate Fit Statistics
#'
#' Computes the "fit" statistics for an n-parameter logistic model
#'
#' @param y Numeric. The y-values of the data
#' @param yfit Numeric. The fitted values based on the n-PL model fit
#' @return A list of fit statistics:
#' \item{Goodness}{The Goodness of Fit for the model}
#' \item{SteErr}{The standard error of the model fit}
#' \item{p.value}{The p-value associated with the model fit}
#' @author Stu Field
#' @seealso \code{\link[stats]{quantile}}
#' @references Spiess and Neumeyer, 2010.
#' @importFrom stats lm pf qt var
#' @noRd
get.npl.stats <- function(y, yfit) {
   w      <- (y - yfit)^2
   lmtest <- summary(stats::lm(y ~ yfit, weights=w))
   fstat  <- lmtest$fstatistic
   p      <- pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
   n      <- sum(w != 0)
   W      <- n / ((n - 1) * sum(w))
   list(Goodness=gof(y, yfit), StdErr=sqrt(W * sum(w^2)), p.value=p)
}


#' @noRd
calc.npl.ci <- function(stdErr, yobs, newy, alpha=0.95) {
   n <- length(yobs)
   ybar <- mean(yobs, na.rm=TRUE)
   t <- qt(1-(1-alpha)/2, n-2)
   ci <- t * stdErr * sqrt((1/n + (newy-ybar)^2/sum((newy-ybar)^2)))
   list(lo=newy-ci, hi=newy+ci)
}



#' Calculate AIC
#'
#' Computes the AIC using a formula for the maximum log-liklihood of a
#' nonlinear model given by Spiess and Neumeyer, 2010.
#' make procedure a little more robust by only using the middle 90%
#'
#' For robust estimation, the AIC approximation is based on the
#' middle 90% of the data
#'
#' @param resids A vector of residuals based on the model fit
#' @param npar Numeric. The number of parameters in the model
#' @return The AIC approximation
#' @author Stu Field & Tom Hraha
#' @seealso \code{\link[stats]{quantile}}
#' @references Spiess and Neumeyer, 2010.
#' @importFrom stats quantile
#' @noRd
calc.aic <- function(resids, npar) {
   q <- quantile(resids, probs=c(0.05,0.95), names=FALSE, na.rm=TRUE)
   resids90 <- resids[ which(resids > q[1] & resids < q[2]) ]
   nr <- length(resids90)
   # maximum log-liklihood
   lnL <- 0.5 * ( -nr*(log(2*pi) + 1 - log(nr) + log(sum(resids90^2))) )
   # standard AIC approximation
   2*npar - 2*lnL
}



#' S3 Plot Method
#'
#' S3 plot method for class "npl"
#'
#' @rdname fit.npl
#' @param x An object of class "npl"
#' @param bg The background color for the data points
#' @param pch The character shape value for the data points (should
#' be compatible with the "bg" argument
#' @param pt.col The color for the edge of the points (default black)
#' @param add.ci Logical. Should 95% confidence interval (based on standard
#' error) be overlaied with a polygon shading?
#' @param add.infl Logical. Should the inflection point be included on the curve?
#' @return A plot
#' @seealso \code{\link[graphics]{curve}}
#' @importFrom graphics curve points
#' @method plot npl
#' @noRd
plot.npl <- function(x, ..., bg = ggplot2::alpha("gray30", 1/3),
                     pch = 21, pt.col = 1, add.ci = FALSE,
                     add.infl = TRUE) {

  with(get_pars(x),
       curve( U + ( L - U ) / ( 1 + (x / k)^b ),
             from = 1e-05, to = 1e+05,
             ylim = c(0, 6), log = "x", ...))
  graphics::points(x$data, col = pt.col, bg = bg,
                   pch = pch, lwd = 0.5, cex = 1)

  if ( add.ci ) {
    ci95 <- calc.npl.ci(x$stats$StdErr, x$data$y, x$newy)
    plotPolygon(list(x$newx, ci95$hi), list(x$newx,ci95$lo),
                add = TRUE,
                col = ggplot2::alpha("skyblue", 0.5))
  }

  if ( add.infl ) {
    graphics::points(x$pt.infl, pch = 18, cex = 2,
                     col = ggplot2::alpha("darkred", 0.75))
  }

  theta_data <- sprintf("%s = %s", c(names(x$pars), "AIC"),
                        format(c(x$pars, x$stats$AIC), digits = 2))
  graphics::legend("topleft", legend = theta_data, ncol = 1, bg = NA,
                   cex = 0.75, box.lty = 0, title = x$npar)
}



#' S3 Print Method
#'
#' A generic print call for objects of the "npl" class.
#' Objects are created by a call to \code{\link{fit.npl}}.
#'
#' @rdname fit.npl
#' @seealso \code{\link{cat}}
#' @importFrom purrr map2
#' @method print npl
#' @export
print.npl <- function(x, ...) {
  cat("* n-Parameter Logistic Fit:", x$npar, "\n")
  sapply(names(x$pars), function(.x)
         cat(sprintf("     %s = %0.3f\n", .x, get_pars(x)[[.x]])))
  cat("* Statistics:\n")
  purrr::map2(names(x$stats), c(1, 3, 2, 6), function(.x, .y)
              cat(sprintf("     %s%s= %0.3f\n", .x, strrep(" ", .y), x$stats[[.x]])))
  cat(sprintf("  Fitted points ............... %i\n", nrow(x$data)))
  cat(sprintf("  Function Min ................ %0.3f\n", x$minimum))
  cat(sprintf("  Iterations .................. %i\n", x$iterations))
  cat(sprintf("  Convergence ................. %s\n", ifelse(x$convergence == 0, "Yes", "Error")))
  cat(sprintf("  Inflection Point ............ (%s)\n", paste(round(x$pt.infl, 3),collapse = ", ")))
  cat(sprintf("  Linear RFU Limits ........... (%s)\n", paste(round(unlist(x$limits), 4), collapse = ", ")))
  cat(sprintf("  Outliers .................... %s\n", x$outliers))
  invisible(x)
}




#' Calculate Curve Limits
#'
#' Calculates the "bend" points of the logistic curve where the non-linear
#' portions of the curve begin/end. The linear region is preferred for mapping
#' RFU values onto the dilutions axis. Values outside the linear range
#' should not be mapped and NA returned.
#'
#' @param par.list A *named* list of parameters named L, U, k, b, and if par=5, s.
#' @return A list object containing:
#' \item{upperlim}{The RFU value of the upper limit "bend"}
#' \item{lowerlim}{The RFU value of the lower limit "bend"}
#' @note This was ported over from the MatLab code
#' @author Stu Field & Tom Hraha
#' @seealso \code{\link[base]{with}}
#' @examples
#' p <- c(0.5, 6.2, 1.9, 0.3)
#' names(p) <- c("L","U","k","b")
#' calc.limits(p)
#' @noRd
calc.limits <- function(par.list) {
  Kval <- 4.6805
  with(par.list,
       list(lowerlim = U + ( (L-U) / (1+(1/Kval)) ),
            upperlim = U + ( (L-U) / (1+Kval) ))
       )
}



# vec is a vector of RFU values corresponding to a specific SOMAmer
calc.mapped.scale.factors <- function(vec, par.list) {
  par.list$k <- 1.00
  limits     <- calc.limits(par.list)
  vec[ vec < limits$lowerlim | vec > limits$upperlim ] <- NA
  .n4PLinv(par.list, vec)
}


# data is a named data frame of ONLY the features to be used in the normalization
calc.dil.scale.factors <- function(data, theta) {

  features <- names(data)
  m <- getSeqIdMatches(features,rownames(theta))
  theta <- theta[ m[, 2], ]
  rownames(theta) <- m[, "features"]
  print(dim(data))
  print(dim(theta))
  stopifnot(all(ncol(data) == nrow(theta)))
  print(head(theta))
  predicted.dil.mat <- sapply(features, function(.f)
                              calc.mapped.scale.factors(data[[.f]], as.list(theta[.f,])))
  #print(predicted.dil.mat)
  if ( nrow(predicted.dil.mat) != nrow(data) ) {
    stop("Problem detected in the dimensions of the input data frame of features and the output scale factors.")
  }

  predicted.dil <- apply(predicted.dil.mat, 1, median, na.rm = TRUE)
  global.median <- median(predicted.dil.mat,na.rm = TRUE)
  data.frame(PredictedDilution    = predicted.dil,
             ScaleFactorPredicted = global.median / predicted.dil,
             ScaleFactorAux       = 1 / predicted.dil,
             NAconfidence         = apply(predicted.dil.mat,1,function(.x) sum(!is.na(.x))/length(.x)))
}


dilutionNormalize <- function(adat) { }


get.4PLtheta.file <- function(file) {
  x     <- read.csv(file, row.names=1, stringsAsFactors=FALSE)
  x     <- x[ order(x$AIC, decreasing=FALSE), ]
  names <- x$SeqIds %>% cleanNames
  x <- x[, intersect(names(x),c("L","U","k","K","b")) ] %names%
    c("L","U","k","b")
  rownames(x) <- names
  return(x)
}


get_tom_data <- function(x) {
  df <- read.csv(x)[, -1]
  nm <- stringr::str_remove_all(names(df), "\\_[xy]$")
  lapply(seq(1,19,2), function(.i)
         df[,c(.i,.i+1)] %names% c("x","y") %>% na.omit) %names% unique(nm)
}

