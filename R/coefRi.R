#' Calculate Interclass Correlation Coefficient (ICC)
#' 
#' The Intraclass correlation coefficient (r_i; aka ICC) can be used to
#' estimate the repeatability of a method. The value 0 -> 1. Depending on how
#' the groups are set up, you want all your variation to be among groups
#' (individuals), not within groups (repeats) so you want this value to be high
#' if individuals are your groups, and low if your repeated measurements are
#' the groups. When ICC is high, it means most of the variation is
#' *between* treatment groups.
#' 
#' @param x A matrix or data frame containing the raw data of the various
#' treatments you are testing in the ANOVA.
#' @param groups A vector of the factor groupings for `x`.
#' @return A list containing:
#' \item{model}{Resulting ANOVA table}
#' \item{ICC}{The intraclass correlation coefficient, the measure of
#'   similarity among individuals \emph{within} a treatment
#'   group relative to the differences found \emph{among} groups}
#' @author Stu Field
#' @seealso [aov()]
#' @references Sokal & Rohlf (Biometry; 3rd ed.) 210-214. Sokal & Rohlf
#' (Biometry; 2rd ed.) 211-216.
#' @examples
#' head(Ri_data)    # internal data
#' x <- as.vector(as.matrix(Ri_data))[ !is.na(as.vector(as.matrix(Ri_data))) ]
#' coefRi(x, groups = rep(names(Ri_data), c(8, 10, 13, 6)))
#' @importFrom stats aov
#' @export
coefRi <- function(x, groups) {

  groups <- factor(groups)
  model  <- summary(stats::aov(x ~ groups))

  # Calculate average sample size
  n_i <- table(groups)
  n_o <- (1 / (length(n_i) - 1)) * ( sum(n_i) - (sum(n_i^2) / sum(n_i)) )

  # Calculate r_i
  MS_among  <- model[[1L]][1L, "Mean Sq"]
  MS_within <- model[[1L]][2L, "Mean Sq"]
  s_Asq <- (MS_among - MS_within) / n_o
  r_i   <- s_Asq / (MS_within + s_Asq)
  list(Model = model, ICC = r_i)
}
