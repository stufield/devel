# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
##################################
#    Function:   duplicatedIndex
##################################

#' Determine Duplicated Entries
#'
#' Calculates the index/indices of the duplicated entries
#' of a vector (string or numeric).
#'
#' @param x A vector containing (possibly) duplicated entries.
#' @return A vector of the indices of the duplicated entries.
#' @author Stu Field
#' @seealso \code{\link[base]{duplicated}}
#' @examples
#'
#' duplicatedIndex(c(1, 1:10))
#' duplicatedIndex(c("a", "a", "b","c"))
#'
#' @export duplicatedIndex
duplicatedIndex <- function(x) {
   dupes <- duplicated(x) | duplicated(x, fromLast=TRUE)
   which(dupes)
}

