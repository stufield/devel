
#' Summarize dimensions of list of data frames
#'
#' Returns the dimensions of the objects contained within a list of
#' objects, typically a list of data frame objects.
#'
#' @param x A list of objects to be evaluated using `dim`.
#' @return A list of dims each of the dimensions in the
#' elements of the data list.
#' @author Stu Field
#' @seealso \code{\link{dim}}, \code{\link{lapply}}
#' @examples
#' tmp <- lapply(c(2, 3, 6, 8, 12), function(x) matrix(1:24, ncol = x))
#' names(tmp) <- head(letters, 5)
#' data_dims(tmp)
#' @importFrom rlang signal
#' @importFrom purrr compact
#' @export data_dims
data_dims <- function(x) {
  if ( is.null(names(x)) ) {
    rlang::signal("`x` must be a *named* list of data frames.", "error")
  }
  dim_list <- lapply(x, dim) %>%
    purrr::compact()
  do.call(rbind, dim_list) %>%
    data.frame() %>%
    magrittr::set_names(c("rows", "columns")) %>%
    dplyr::mutate(meta = sapply(x, getMeta, n = TRUE),
                  apts = sapply(x, getAptamers, n = TRUE))
}
