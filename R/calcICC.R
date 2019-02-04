
#' Calculate Intraclass Correlation Coefficient
#'
#' Calculate Intraclass Correlation Coefficient (r_i).
#'
#' @param x A named list containing vectors of data representing the
#' various groups to be tested. One list element for each group.
#' @param do.log Logical. Should values be log-transformed.
#' @param alpha The significance level for the test.
#' @return A data frame of the ANOVA table. A list of the values for the ICC.
#' @author Stu Field
#' @seealso \code{\link{sapply}}, \code{\link{mean}}, \code{\link{sum}}, \code{\link{aov}}, \code{\link{qf}}
#' @references Sokal and Rohlf, pg. 214, 2nd ed.
#' @examples
#'
#' @importFrom rlang signal
#' @export calcICC
calcICC <- function(x, do.log = FALSE, alpha = 0.05) {

  if ( inherits(x, "data.frame") ) {
    x %<>% as.list()
  }

  x <- lapply(x, function(n) n[!is.na(n)])

  if ( do.log ) {
    x <- lapply(x, log10)
  }

  group_n  <- sapply(x, length)   # vector of n
  group_mu <- sapply(x, mean)
  df1 <- length(x) - 1
  df2 <- sum(group_n) - length(x)
  n_o <- (1 / (length(x) - 1)) * (sum(group_n) - (sum(group_n^2) / sum(group_n)))

  if ( n_o > mean(group_n) ) {
    rlang::signal(
      "n_o > mean sample size: check calculation & sample sizes.", "error")
  }

  ss_within <- sum(sapply(x, function(.i) sum((mean(.i) - .i)^2) ))
  # calc SS treatment means weighted by n of each group
  ss_between <- sum(group_n * (group_mu - mean(unlist(x)))^2)

  ms_within  <- ss_within / (sum(group_n) - length(x))
  ms_between <- ss_between / (length(x) - 1)
  s2_between <- (1 / n_o) * abs(ms_between - ms_within)

  Fstat <- ms_between / ms_within
  a     <- alpha / 2
  Fc1   <- qf(1 - a, df1, df2)
  Fc2   <- qf(1 - a, df2, df1)
  Fc3   <- qf(1 - a, df1, Inf)
  Fc4   <- qf(1 - a, Inf, df1)

  # CI95 for s2_between
  ci_lo <- ms_within / n_o * ( Fstat / Fc3 - 1 - (Fc1 / Fc3 - 1) * Fc1 / Fstat)
  ci_hi <- ms_within / n_o * ( Fstat * Fc4 - 1 + (1 - Fc4 / Fc2) *  1/ (Fc2 * Fstat))
  ci95  <- c(ci_lo, ci_hi) %>% purrr::set_names(c("2.5%", "97.5%"))
  ci95[ ci95 < 0 ] = 0

  aov_tab <- data.frame(
    SourceVariation = c("Between Group", "Within Group", "Total"),
    df = c(df1, df2, df1 + df2),
    SS = c(ss_between, ss_within, ss_within + ss_between),
    MS = c(ms_between, ms_within, NA),
    Fstat = c(Fstat, NA, NA),
    Fcrit = c(qf(1 - 0.05, df1, df2), NA, NA),
    p.value = c(1 - pf(ms_between / ms_within, df1, df2), NA, NA)
  )

  list(anova           = aov_tab,
       s2.within       = ms_within,
       s2.between      = s2_between,
       CI95.s2.between = ci95,
       icc             = s2_between / (s2_between + ms_within))

}
