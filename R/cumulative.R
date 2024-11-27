#' Cumulative Wrapper
#'
#' Returns cumulative vector values of neighboring vector elements.
#'
#' @param x A numeric vector.
#' @return A list containing:
#' \item{cum_min}{The cumulative minima of the elements of `x`}
#' \item{cum_max}{The cumulative maxima of the elements of `x`}
#' \item{sum}{The sum of the elements of `x`}
#' \item{cum_sum}{The cumulative sum of the elements of `x`}
#' \item{prod}{The product of the elements of `x`}
#' \item{cum_prod}{The cumulative product of the elements of `x`}
#' @author Stu Field
#' @seealso [cumsum()], [cumprod()]
#' @examples
#' \dontrun{
#' ?cumsum
#' }
#' cumulative(1:10)
#' @export
cumulative <- function(x) {
  list(cum_min  = cummin(x),
       cum_max  = cummax(x),
       sum      = sum(x),
       cum_sum  = cumsum(x),
       prod     = prod(x),
       cum_prod = cumprod(x)
  )
}
