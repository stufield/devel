
#' Get Duplicated Index List
#'
#' Calculate the indices of the duplicated values of a vector
#' and return a list corresponding to the duplicated groups.
#'
#' @param x A vector of values with potentially duplicated values.
#' @return A list of indices split by duplicated value of any duplicated values.
#' @author Stu Field and Yolanda Hagar
#' @examples
#' x <- rep(head(LETTERS, 10), c(1,1,2,3,1,2,3,1,2,1))
#' x
#' table(x)
#' getDupIdxList(x)
#'
#' getDupIdxList(1:10)         # no duplicates (empty list)
#' getDupIdxList(LETTERS)      # no duplicates (empty list)
#' getDupIdxList(c(1, 1:10))   # 1 duplicate
#' @seealso \code{\link{duplicatedIndex}}
#' @importFrom purrr set_names
#' @export getDupIdxList
getDupIdxList <- function(x) {
  duplicatedIndex(x) %>% x[.] %>% unique() %>% {
    purrr::map(., function(id) which(x == id)) %>% purrr::set_names(.)}
}
