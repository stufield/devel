#' Pairwise Comparison Tables
#'
#' This is a pairwise wrapper for \code{\link{createTestsList}}, which
#' calculates a statistics table for each pairwise group set that
#' can be calculated from a given set of factor levels. There are
#' typically \code{choose(n, 2)} pairwise comparisons to make and
#' thus \code{Nchoose2} tables are generated.
#'
#' @param dat A data set or "soma.adat" object containing RFU values
#' and a \code{group.field} column containing a vector of levels
#' to compare in each data split. The levels of this field should
#' be \code{>2}.
#' @param group.field Character. The name of the column in the
#' ADAT containing the grouping information to split the data into
#' pairwise groups.
#' @param comps A n x 2 matrix of the group name comparisons you wish to make.
#' Defaults to all pairwise \code{choose(n, 2)} comparisons.
#' @param ... Arguments passed to \code{\link{createTestsList}}
#' @return A list of statistacal tables, each the result of a
#' call to the specified function on a pairwise split of the data.
#' @author Stu Field
#' @seealso \code{\link{createTestsList}}, \code{\link[utils]{combn}}, \code{\link[SomaClassify]{createTrainingData}}
#' @examples
#' tmp <- sample.adat
#' tmp$newGroup <- rep(head(LETTERS, 4), each = 5)
#' tests <- calcPairwiseTables(tmp, group.field = "newGroup", test = "t", do.log = TRUE)
#' names(tests)
#' @importFrom utils combn
#' @export
calcPairwiseTables <- function(dat, group.field, comps = NULL, ...) {

  zap <- function(.x)
    stringr::str_replace_all(.x, "[^A-Za-z0-9]", ".")

  if ( is.null(comps) )
    comps <- unique(dat[[group.field]]) %>%
      sort() %>%
      utils::combn(2) %>%
      t()

  pair_names <- apply(comps, 2, zap) %>%
    apply(1, paste0, collapse = "_vs_")

  # set up list of argument lists of createTrainingData to pass to do.call()
  apply(comps, 1, function(comp) {
        args <- list()
        args$data <- dat
        args[[group.field]] <- as.list(comp)
        args$class1 <- comp[1]
        args$class2 <- comp[2]
        return(args)
  }) %>%
  stats::setNames(pair_names) %>%
  # apply the do.call() with each arg list to createTrainingData()
  # creates a list of training data objects by pairwise group
  lapply(., do.call, what = createTrainingData) %>%
  # pass list of subset data sets to the final function
  # statistical test args thru the '...'
  createTestsList(...)

}

