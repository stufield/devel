
#' 1 Step EM
#'
#' A single step of the EM algorithm
#'
#' @noRd
#' @param y value
#' @param mu1 value
#' @param mu2 value
#' @param sd1 value
#' @param sd2 value
#' @param pi.hat value
#' @return return_value
#' @author Stu Field
em_1_step <- function(y, mu1, mu2, sd1, sd2, pi.hat) {

  # responsibilities of distribution #2
  gamma      <- pi.hat * dnorm(y, mu2, sd2) / ((1-pi.hat) * dnorm(y, mu1, sd1) +
    pi.hat * dnorm(y, mu2, sd2))
  new.mu1    <- sum((1 - gamma) * y) / sum(1-gamma)
  new.mu2    <- sum(gamma*y) / sum(gamma)
  new.var1   <- sum((1 - gamma) * (y - mu1)^2) / sum(1 - gamma)   # this is variance
  new.var2   <- sum(gamma*(y - mu2)^2) / sum(gamma)         # this is variance
  new.pi.hat <- mean(gamma)
  LL         <- log( (1 - new.pi.hat) * dnorm(y, new.mu1, sqrt(new.var1)) +
                    new.pi.hat * (dnorm(y, new.mu2, sqrt(new.var2))) )

  list(mu = c(new.mu1, new.mu2),
       sigma = c(sqrt(new.var1), sqrt(new.var2)),
       pi.hat = new.pi.hat,
       loglik = sum(LL),
       responsibilities_2 = gamma)
}



#' Choose Initial Conditions
#'
#' Description
#'
#' @noRd
#' @param y value
#' @param k value
#' @return return_value
#' @author Stu Field
choose_init <- function(y, k = 2) {
  bins   <- split(y, sample(1:k, length(y), replace = TRUE))
  emp.mu <- sapply(bins, mean)
  emp.sd <- sapply(bins, sd)
  if ( any(emp.sd == 0) ) {
    emp.sd[which(emp.sd==0)] = runif (sum(emp.sd == 0), 0, sd(data))
  }
  sigma.k <- 1/rexp(k, rate = emp.sd)
  mu.k <- rnorm(k, mean = emp.mu, sd = sigma.k)
  list(mu = mu.k, sigma = sigma.k, pi.hat = runif(1))
}



#' Title
#'
#' Description
#'
#' Details
#'
#' @param data value
#' @param pars Values for start.sd, start.pi, max.iter, max.restarts, and eps
#' @return A list containing:
#' @author Stu Field
#' @references Tibshirani and Hastie; Bible
#' @examples
#' x <- c(rnorm(50,mean=10),rnorm(50,mean=25))
#' @export normal_k2_mixture
normal_k2_mixture <- function(data, pars = list(start.mu = c(NULL, NULL),
                                                start.sd = c(NULL, NULL),
                                                start.pi = NULL),
                              max.iter = 1000, max.restarts = 25, eps = 1e-08) {

  good_names <- c("start.mu", "start.sd", "start.pi")

  if ( any(!names(pars) %in% good_names) ) {
    message(paste("Should be:", paste(good_names, collapse = ", ")))
    message(paste("   Names are:", names(pars)))
    rlang::signal("Check spelling of list names for `pars =` argument."
                  "error")
  }

  pars <- purrr::map(good_names, function(x) {
     if ( x %in% names(pars) ) {
       pars[[x]]
     } else {
       NULL
     }
    }) %>%
    magrittr::set_names(good_names)

  if ( any(sapply(pars, is.null)) ) {
    tmp.pars <- choose_init(y = data)
  }

  mu.par    <- if ( any(is.null(pars$start.mu)) ) tmp.pars$mu else pars$start.mu
  sigma.par <- if ( any(is.null(pars$start.sd)) ) tmp.pars$sigma else pars$start.sd
  pi.par    <- if ( is.null(pars$start.pi) ) tmp.pars$pi.hat else pars$start.pi

  iter       <- numeric(1)
  loglik     <- numeric(1)
  loglik.vec <- numeric(0)
  restarts   <- numeric(1)
  dll        <- 1 + eps

  while ( dll > eps ) {
    iter      <- iter + 1
    tmp       <- em_1_step(y = data,
                           mu1 = mu.par[1],
                           mu2 = mu.par[2],
                           sd1 = sigma.par[1],
                           sd2 = sigma.par[2],
                           pi.hat = pi.par)
    mu.par    <- tmp$mu
    sigma.par <- tmp$sigma
    pi.par    <- tmp$pi.hat
    dll       <- abs(loglik - tmp$loglik)
    loglik.vec[iter] <- tmp$loglik
    loglik    <- tmp$loglik
    if ( iter >= max.iter || min(sigma.par) < 1e-06 ) {
      message(" No convergence ... OR ... One of the variances is going to zero.")
      message(" Restarting with new initial conditions.")
      new.pars  <- choose_init(y = data)
      mu.par    <- new.pars$mu
      sigma.par <- new.pars$sigma
      pi.par    <- new.pars$pi.hat
      restarts  <- restarts + 1
      if ( restarts > max.restarts ) {
        rlang::signal(
          "Too many restarts. Possible extreme outliers in distribution.",
          "error")
      }
      iter   <- 0
      loglik <- 0
      loglik.vec <- numeric(0)
      dll <- 1 + eps
    }
  }

  message(" Iteration ...", iter, "\n")

  list(y = data,
       mu = as.numeric(mu.par),
       sigma = as.numeric(sigma.par),
       pi.hat = as.numeric(pi.par),
       lambda = c(1 - pi.par,pi.par),
       loglik = as.numeric(loglik),
       loglik.vec = loglik.vec,
       niter = iter,
       restarts = restarts,
       posterior = tmp$responsibilities_2,
       fn = "normal_k2_mixture") %>%
    addClass("mix_k2")
}



#' Plot Object
#'
#' S3 method for "mix_k2" objects
#'
#' @rdname normal_k2_mixture
#' @param x A `mix_k2` object generated from \code{\link{normal_k2_mixture}}
#' @param type Character. Matched string one of: "density", "likelihood" or "posterior".
#' @param title Character. Title for the plot.
#' @param ... Passed to \code{\link{hist}}.
#' @author Stu Field
#' @references See Tibshirani and Hastie ("bible"); pg. 273.
#' @examples
#' plot(x)
#' @importFrom graphics plot par lines hist
#' @method plot mix_k2
#' @export
plot.mix_k2 <- function(x, type = c("density", "likelihood", "posterior"),
                        title = NULL, ...) {

  type <- match.arg(type)
  k    <- length(x$mu)
  par(par.def)

  if ( type == "density" ) {
    if ( is.null(title) ) {
      title <- "Density Histogram"
    }
    sort.y <- sort(x$y)
    hist(sort.y, prob = TRUE, main = title, xlab = "Data",
         col = "gray80", breaks = 20, ...)
    lines(density(sort.y), lty = 2, lwd = 1)
    #box()
    for ( i in 1:k ) {
      prob.vec <- dnorm(sort.y, mean = x$mu[i], sd = x$sigma[i])
      lines(sort.y, x$lambda[i] * prob.vec, col = c(2, 4)[i], lwd = 1.5)
    }
    addLegend(sprintf("n=%i", length(x$y)), cols = NULL, "topright",
              cex = 0.9, box.lty = 0)

  } else if ( type == "likelihood" ) {

    plot(x$loglik.vec, main = "Log-likelihood Trajectory", type = "b",
         ylab = "Log-Likelihood", xlab = "Iteration", lty = 2, col = "dodgerblue",
         pch = 21, bg="darkred", lwd = 1.5, cex = 1.2)
    grid(col = "gray75")

  } else if ( type == "posterior" ) {

    max <- max(1, hist(x$y, plot = FALSE)$density)
    hist(x$y, prob = TRUE, main = "Posterior Responsibilities", xlab = "Data",
         breaks = 20, col = "gray80", ylim = c(0, max), ...)
    lines(density(x$y), lty = 2, col = 1, lwd = 1.5)
    lines(sort(x$y), x$posterior[order(x$y)], type = "b", lty = 2, col = "dodgerblue",
          pch = 21, bg = "darkred", lwd = 1.5, cex = 1.2)
    axis(1, at = x$y, labels = NA, col.ticks = 3, lwd.ticks = 2, tcl = 0.5)

  } else {
    rlang::signal(
      paste("Invalid `type =` argumnet passed to `plot()`.",
            type),
      "error")
  }

}



#' Calculate Equal Probability
#'
#' Calculate the point of equal likelihood between 2 distributions
#'
#' @rdname normal_k2_mixture
#' @return A scalar
#' @author Kirk DeLisle
#' @examples
#' equal.likelihood.pt(x)
#' @export
equal.likelihood.pt <- function(x) {
  stopifnot(inherits(x, "mix_k2"))
  a <- (1 / (2 * x$sigma[1]^2)) - (1/(2*x$sigma[2]^2))
  b <- (x$mu[2] / x$sigma[2]^2) - (x$mu[1] / x$sigma[1]^2)
  c <- (x$mu[1]^2 / (2 * x$sigma[1]^2)) - (x$mu[2]^2 / (2 * x$sigma[2]^2)) -
    (log(x$sigma[2] / x$sigma[1] * x$lambda[1] / x$lambda[2]))
  lik <- numeric(2)
  lik[1] <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
  lik[2] <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
  lik[which(lik > min(x$mu) && lik < max(x$mu))]
}

