#' Perform Quantile Normalization
#'
#' Normalize rows (i.e. samples or arrays) of a data frame to have identical
#' quantiles.
#'
#' This function is intended to normalize intensities between samples (arrays).
#' Each quantile of each column is set to the mean of that quantile across
#' samples. The intention is to make all the normalized columns have an
#' identical empirical distribution. This will be exactly true if there are no
#' missing values and no ties within the columns: the normalized columns are
#' then simply permutations of one another.
#'
#' If there are ties amongst the intensities for a particular array, then with
#' \code{ties=FALSE} the value of the lowest mean is used for all ties. If
#' \code{ties=TRUE}, all the tied values for that sample will be normalized to
#' the same value, the mean of the quantiles for the tied values.
#'
#' @param x A data frame (or matrix) with samples as the rows and
#' genes/proteins/features as columns.
#' @param ties Logical. If \code{TRUE}, ties in each column of \code{x} are
#' treated carefully. Tied values will be normalized to the mean of the
#' corresponding pooled quantiles.
#' @return A data frame of the same dimensions as "x" containing the normalized
#' values.
#' @note Results in identical output as \code{normalizeBetweenArrays} from the
#' \code{limma} package.
#' @author Stu Field
#' @seealso \code{\link[base]{apply}} \cr \code{\link[base]{rank}} \cr
#' \code{\link[base]{duplicated}} \cr \code{\link[base]{mean}}
#' @references Bolstad, B. M., Irizarry R. A., Astrand, M., and Speed, T. P.
#' (2003), A comparison of normalization methods for high density
#' oligonucleotide array data based on bias and variance. Bioinformatics 19,
#' 185-193.
#' @examples
#' # example from http://en.wikipedia.org/wiki/Quantile_normalization
#' # the sample does not use the same ties method
#' m <- matrix(c(5,4,3,2,1,4,3,4,6,4,2,8), ncol = 4)
#' m <- data.frame(m, row.names = sprintf('sample%s',letters[1:3]))
#' names(m) <- sprintf('gene%s', LETTERS[1:4])
#' quantileNormalize(m)
#' quantileNormalize(m, ties = FALSE) # matches Wikipedia example (transposed)
#' @export
quantileNormalize <- function(x, ties = TRUE) {

  if ( !inherits(x, "matrix") ) {
    out <- as.matrix(x)
  } else {
    out <- x
  }

  order_x    <- t(apply(x, 1, rank, ties.method = "min"))
  sorted_x   <- t(apply(x, 1, sort))
  rank_means <- apply(sorted_x, 2, mean, na.rm = TRUE)

  for ( i in seq(length(rank_means)) ) {
    idx <- which(order_x == i, arr.ind = TRUE)
    if ( any(duplicated(idx[, 1L])) && ties ) {
      dupes     <- duplicated(idx[, 1L]) | duplicated(idx[, 1L], fromLast = TRUE)
      idx_dupes <- idx[ dupes, ]
      idx       <- idx[ !dupes, ]
      out[idx_dupes] <- mean(rank_means[i:(i + sum(dupes) - 1)])
    }
    out[idx] <- rank_means[i]
  }
  data.frame(out)
}
