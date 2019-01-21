# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
########################################
#         Function:   seriateHeatmap
########################################

#' Heatmap Seriation
#'
#' Produce a heatmap of all SOMAmers in an adat. Just a black and white total
#' heatmap of ordered, correlated analytes.
#'
#' @param data The "soma.adat" object containing aptamer data.
#' @param method Which clustering method to use, see \code{\link[seriation]{seriate}}.
#' @param censor.crit Logical. Should the correlation values be cut off at a
#' critical value.
#' @return A seriated heatmap of the entire data frame.
#' @author Stu Field
#' @examples
#' seriateHeatmap(sample.adat)
#' @importFrom seriation seriate
#' @export seriateHeatmap
seriateHeatmap <- function(data, method = "OLO", censor.crit = FALSE) {
  data     <- data[, sapply(1:ncol(data), function(col) is.numeric(data[, col]))]
  t_crit   <- qt(p = 1 - (0.05 / 2), df = nrow(data) - 2)   # calc crit t-value
  cor_crit <- t_crit / sqrt(t_crit^2 + nrow(data) - 2)      # calc crit Rho based on t-value
  dflog    <- apply(data, 2, log, base = 10)                # take log10
  cor_mat  <- cor(data, method = "spearman")                # calc correlation matrix
  my_order <- seriation::seriate(dist(cor_mat), method = method)
  if ( censor.crit ) {
     cor_mat[ cor_mat < cor_crit ] <- 0                     # set all correlations less than crit = 0
  }
  pimage(dist(cor_mat), my_order)
}
