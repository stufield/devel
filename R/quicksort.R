#' Quick Sort Algorithm
#'
#' The quick-sort algorithm is a computer science "divide-and-conquer" algorithm
#'   used here as a applied example of recursion.
#'   Sometimes called partition-exchange sort, is an efficient sorting algorithm.
#'   Developed by British computer scientist Tony Hoare in 1959 and published
#'   in 1961, it is still a commonly used algorithm for sorting.
#'   When implemented well, it can be about two or three times faster than its
#'   main competitors, `merge-sort` and `heapsort`.
#'
#' It works by selecting a 'pivot' element from the array and partitioning
#'   the other elements into two sub-arrays, according to whether they are less
#'   than or greater than the pivot. The sub-arrays are then sorted recursively.
#'   This can be done in-place, requiring small additional amounts of memory
#'   to perform the sorting.
#'
#' @param x An vector to be sorted via the quick-sort algorithm.
#'
#' @return A sorted vector or `x`.
#' @references tony Hoare (1959).
#' @source \url{https://en.wikipedia.org/wiki/Quicksort}
#'
#' @examples
#' x <- withr::with_seed(101, sample(LETTERS))
#' quicksort(x)
#'
#' # numeric x; compared to `base::sort()`
#' # this implementation is *much* slower
#' y <- sample(1:1000)
#' bench::mark(
#'   base      = sort(y),
#'   quicksort = quicksort(y)
#' )
#' @export
quicksort <- function(x) {
  if ( length(x) == 0 ) {           # if at max/min of array
    return(NULL)
  } else if ( length(x) == 1L ) {   # if recursion hits bottom single element
    return(x)
  }
  partition(x, choose_pivot(x))
}

choose_pivot <- function(p) {
  # use "middle-of-three" rule for choosing pivot
  idx <- c(1L, floor(length(p) / 2), length(p))
  stats::median(p[idx])
}

partition <- function(x, pivot) {
  lo <- which(x < pivot)
  hi <- which(x > pivot)
  L  <- sum(x == pivot)
  if ( L > 1L ) pivot <- rep(pivot, L)   # catch ties
  left  <- quicksort(x[lo])
  right <- quicksort(x[hi])
  c(left, pivot, right)
}
