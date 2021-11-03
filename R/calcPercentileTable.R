#' Calculate percentiles
#'
#' Calculate percentiles of a series of vectors and put
#' into tablular form.  By default, the 5th, 25th, 50th, 75th,
#' and 95th quantiles are generated, though any quantiles can be displayed.
#' Often used for new matrix characterization of CVs.
#'
#' @param x A *named* list of vectors.
#' @param probs Numeric. A vector of probabilities in [0,1].
#' @return A table of (usually CVs) split by percentile.
#' @seealso \code{\link{quantile}}
#' @author Stu Field
#' @examples
#' x <- lapply(seq(50, 250, by = 50), function(x) rnorm(100, mean = x))
#' names(x) <- head(LETTERS, length(x))
#' calcPercentileTable(x)
#' @importFrom stats quantile
#' @export
calcPercentileTable <- function(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  if ( !inherits(x, "list") || is.null(names(x)) ) {
    stop(
      paste("The `x =` argument must be a *named* list.", names(x)),
      call. = FALSE)
  }
  lapply(x, function(.x) quantile(.x, probs = probs)) %>%
    data.frame()
}
