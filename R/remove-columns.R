#' Remove Column Containing Specified Entry(s)
#'
#' Remove an entire column of a data table whose
#'   entries contain a specified value. To remove
#'   *rows* with a matching pattern, use [dplyr::filter()].
#'
#' @param x A matrix or data frame. Any 2-dimensional object in R.
#' @param search The exact match to be searched.
#'   If contained within any column, it will be removed.
#'
#' @return Same class object as `x` with possibly column(s) removed.
#'
#' @author Stu Field
#' @examples
#' M <- matrix(1:25, ncol = 5L)
#' M
#' remove_columns(M, search = 20)
#' N <- data.frame(M)
#' N[4L, 4L] <- "A"
#' N
#' remove_columns(N, search = c("A", 17, 23))
#' @export
remove_columns <- function(x, search) {
  .logic <- function(.x) !any(.x %in% search)
  x[, apply(x, 2, .logic)]
}

