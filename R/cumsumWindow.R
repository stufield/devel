#' Calculate Limited Cumulative Sum
#' 
#' Calculate the cumulative sum of a set of numbers within a vector. The
#' difference between this and [cumsum()] is that it is a
#' sliding window approach, so sums are not necessarily calculated over the
#' entire length of the vector. When `cut == length(x)` then it is the
#' same as [cumsum()]. Also, for entries `<` window, i.e.
#' the beginning, the entries returned will be identical to [cumsum()].
#' 
#' If `window >= length(x)`, a warning is triggered and
#' `cumsumWindow` reverts to [cumsum()].
#' 
#' @param x The vector to be summed across.
#' @param window The length/size of the window to sum 
#' within (the moving  cutoff).
#' @return A vector of the sums of the sliding window for the cumulative sums.
#' @author Stu Field
#' @seealso [cumsum()]
#' @examples
#' cumsumWindow(1:20, 5)
#' cumsumWindow(1:20, window = 20)
#' cumsum(1:20)
#' r.vec <- sample(1:20, 100, replace=TRUE) # random vector
#' cumsumWindow(r.vec, 5)
#' @export
cumsumWindow <- function(x, window) {
  hi <- (window + 1):length(x)
  lo <- hi - (window - 1)

  if ( window >= length(x) ) {
    warning("window >= length(x)!", call. = FALSE)
    cumsum(x)
  } else {
    one <- cumsum(x[1:window])
    two <- sapply(1:length(hi), function(i) sum(x[lo[i]:hi[i]]) )
    c(one, two)
  }
}
