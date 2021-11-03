#' Perform Robust PCA
#'
#' Need more description here, perhaps consult Mike Mehan on
#' the reference and how this procedure is justified and validated.
#'
#' @param x A data matrix, typically of RFU data. No meta data.
#' @param tolerance The tolerance for the error of the Euclidean norm
#' of the matrix used to stop the iterations. Iterations are maxed at 250
#' by default.
#' @param max.iter Integer. Maximum number of iterations to perform.
#' @param verbose Logical. Should iteration "Rounds" be displayed at
#' the console as the fitting progresses? Also determines whether a plot
#' of the matrix error in the Euclidean norm decreases as the
#' iterations progress.
#' @return A list of:
#' \item{L }{A matrix same dimensions as `x` and *very* close values.}
#' \item{S }{Shrinkage matrix? At any rate, a *very* sparse matrix.}
#' @author Mike Mehan
#' @examples
#' data <- data.matrix(sample.adat[, getAptamers(sample.adat)])
#' pca1 <- robustPCA(data)    # basic
#'
#' # Run until tolerance is reached & plot
#' pca2 <- robustPCA(data, max.iter = 1000, verbose = TRUE)
#' @export
robustPCA <- function(x, tolerance = 1e-07, max.iter = 250,
                      verbose = getOption("verbose")) {

  x %<>% as.matrix()
  S.cur  <- matrix(0, nrow = nrow(x), ncol = ncol(x))
  Y.cur  <- matrix(0, nrow = nrow(x), ncol = ncol(x))
  mu     <- nrow(x) * ncol(x) / 4 / norm(x, type = "1")
  lambda <- nrow(x)^-0.65   # hard-coded?
  curErr <- 100
  Err    <- numeric(100)
  iter   <- 1

  while ( iter <= max.iter && curErr > tolerance * norm(x, type = "F")  ) {
    L.next <- robustPCAsvdThresh(x - S.cur + mu^-1*Y.cur, mu^-1)
    S.next <- robustPCAshrinkage(x - L.next + mu^-1*Y.cur, mu^-1*lambda)
    Y.next <- Y.cur + mu * (x - L.next - S.next)
    L.cur  <- L.next
    S.cur  <- S.next
    Y.cur  <- Y.next
    curErr <- norm(x - L.cur - S.cur, type = "F")
    message("* Round: ", iter, " error: ", curErr)
    Err[iter] <- curErr
    iter <- iter + 1
  }

  dimnames(L.cur) <- dimnames(S.cur)
  if ( verbose ) {
    plot(Err, type = "l", col = "navy",
         main = "Error in Matrix Euclidean Norm",
         ylim = c(0, 10), lwd = 1.5,
         ylab = "Euclidean Error", xlab = "Iteration")
  }
  list(L = L.cur, S = S.cur)
}


#' Robust PCA SVD Threshold
#'
#' Add function description here ...
#'
#' @param x A data matrix, typically of RFU data.
#' @param tau Passed to robust.pca.shrinkage. See below.
#' @return A numeric value ...
#' @author Mike Mehan
#' @noRd
robustPCAsvdThresh <- function(x, tau) {
  x_svd <- svd(x)
  x_svd$u %*% robustPCAshrinkage(diag(x_svd$d), tau) %*% t(x_svd$v)
}

#' Robust PCA Shrinkage
#'
#' @param x The "d" entry from an \code{\link{svd}} call.
#' @noRd
robustPCAshrinkage <- function(x, tau) {
  x.sign   <- sign(x)
  x.shrink <- abs(x) - tau
  x.shrink[ x.shrink < 0 ] <- 0
  x.sign * x.shrink
}
