#' Print Object to File
#'
#' A thin wrapper around the [sink()] function to print
#'   the corresponding object to a file connection.
#'
#' @param ... Object(s) to be printed to file as standard output.
#' @param file `character(1)`. File name for the output.
#' @param width `numeric(1)`. Controls the maximum number of
#'   columns on a line used in printing vectors, matrices
#'   and arrays, and when filling by [cat()].
#'   See also `getOption("width")`.
#' @examples
#' tab <- cross_tabulate(mtcars, cyl, carb)
#' print_sink(tab, file = "table_file.txt")
#' @export
print_sink <- function(..., file, width = 250) {
  signal_done("Creating text file ... ", file)
  withr::with_local_sink(file)
  withr::with_options(
    list(width = width),
    invisible(lapply(list(...), print))
  )
}
