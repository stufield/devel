#' Summarize dimensions of list of data frames
#'
#' Returns the dimensions of the objects contained within a list of
#'   objects, typically a list of data frame objects.
#'
#' @param x A list of objects to be evaluated using `dim`.
#'
#' @return A list of dims each of the dimensions in the
#'   elements of the data list.
#' @author Stu Field
#'
#' @examples
#' tmp <- lapply(c(2, 3, 6, 8, 12), function(x) matrix(1:24, ncol = x))
#' names(tmp) <- head(letters, 5)
#' data_dims(tmp)
#' @export
data_dims <- function(x) {
  if ( is.null(names(x)) ) {
    stop("`x` must be a *named* list of data frames.", call. = FALSE)
  }
  lapply(x, dim) |>
    helpr::compact_it()
    do.call(what = rbind) |>
    data.frame() |>
    setNames(c("rows", "columns"))
}
