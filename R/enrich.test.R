
#' Test for enrichment
#'
#' Calculated whether `2 x 2` table is enriched
#' for a particular group using Hypergeometric Distribution and
#' the Fisher's Exact test for count data.
#'
#' Can also pass a *named* list containing:
#' \describe{
#'   \item{n11}{The corresponding [1,1] position of the table/matrix.}
#'   \item{n1.}{The sum of the top row of the table.}
#'   \item{n.1}{The sum of the first column of the table.}
#'   \item{n}{The sum of the table.}
#' }
#' @param x A 2x2 confusion matrix (or contingency table) containing
#' the binary decisions for each contingency. Can also be a (named) list
#' containing each of the 4 contingencies. See above.
#' @param alternative Whether to check for "two.sided" (both Enrich/Deplete)
#' or specifically one or the other; "enrich" or "deplete".
#' @return Both the comparison to the Hypergeometric Distribution and
#' The Fisher Exact Test for Count Data with confidence intervals
#' @note Similar result to Fisher Exact test
#' @author Stu Field
#' @seealso \code{\link[stats]{dhyper}}, \code{\link[stats]{fisher.test}}
#' @examples
#' c_mat <- matrix(c(4, 2, 3, 11), ncol = 2)
#' enrich_test(c_mat)
#' en_list <- list(n11 = 4, n1. = 7, n.1 = 6, n = 20)
#' enrich_test(en_list)
#' @importFrom stats fisher.test
#' @importFrom cli rule
#' @importFrom crayon blue
#' @export enrich_test
enrich_test <- function(x, alternative = c("two.sided", "enrich", "deplete")) {

  if ( inherits(x, "matrix") ) {
    n11 <- x[1,1]
    n1. <- sum(x[1, ])
    n.1 <- sum(x[, 1])
    n2. <- sum(x[2, ])
    n   <- sum(x)
  } else if ( inherits(x, "list") & length(x) == 4 ) {
    # x must be passed as list with names:
    if ( is.null(names(x)) ) {
      stop("List must be a *named* list with: 'n11', 'n1.', 'n.1', 'n'.",
           call. = FALSE)
    }

    n11 <- x$n11
    n1. <- x$n1.
    n.1 <- x$n.1
    n   <- x$n
    n2. <- n - n1.
    x   <- matrix(c(n11, n.1 - n11, n1. - n11, n2. - (n.1 - n11)), ncol = 2)
  } else {
    stop("Error in `x` argument. Incorrect format.", call. = FALSE)
  }

  ret <- list()
  prob_vec1 <- stats::dhyper(n11:n.1, n1., n2., n.1)
  one_sided <- sum(prob_vec1)                # sum the probabilities from x11 -> x.1
  prob_vec1[1]         <- prob_vec1[1] / 2
  one_sided_mid        <- sum(prob_vec1)     # half the first probability and sum
  two_sided_double     <- one_sided * 2      # double the one sided p-value
  two_sided_double_mid <- one_sided_mid * 2  # double the one sided p-value
  # ------------------------------------- #
  prob_vec2 <- stats::dhyper(0:n.1, n1., n2., n.1) %>%
    magrittr::set_names(as.character(0:n.1))
  prob_vec2         <- prob_vec2[which(prob_vec2 <= prob_vec2[as.character(n11)])]
  two_sided_min_lik <- sum(prob_vec2)
  prob_vec2[as.character(n11)] <- prob_vec2[as.character(n11)] / 2
  two_sided_min_lik_mid <- sum(prob_vec2)

  ConfusionTable <- x
  dimnames(ConfusionTable) <- list(c("yes", "no"), c("yes", "no"))
  altern <- match.arg(alternative)
  altern <- switch(altern, enrich = "greater", deplete = "less", "two.sided")
  fisher <- stats::fisher.test(ConfusionTable, alternative = altern)

  hyper <- tibble::tribble(
    ~Test,                 ~"p-value",
    "1 sided",             one_sided,
    "2 sided double",      two_sided_double,
    "1 sided mid" ,        one_sided_mid,
    "2 sided double mid",  two_sided_double_mid,
    "Fisher's Exact",      two_sided_min_lik,
    "2 sided min lik mid", two_sided_min_lik_mid
    ) %>%
    dplyr::mutate(
      Preferred = ifelse(Test == "2 sided min lik mid",
                         cli::symbol$star,
                         cli::symbol$dot),
      CI95 = ifelse(Test == "Fisher's Exact",
                    sprintf("(%0.3f, %0.3f)",
                            fisher$conf.int[1],
                            fisher$conf.int[2]), cli::symbol$dot)
    )

  writeLines(cli::rule(crayon::bold("Counts Table"), line_col = crayon::blue))
  print(ConfusionTable)
  cat("\n")
  writeLines(cli::rule(crayon::bold("Tests"), line_col = crayon::blue))
  print(hyper)
  writeLines(cli::rule(line = 2, line_col = crayon::green))
  #ret$hyper[["p-value"]][ ret$hyper[["p-value"]] > 1 ] <- 1

  invisible(list(results = hyper,
                 confusion = ConfusionTable,
                 alternative = altern))
}
