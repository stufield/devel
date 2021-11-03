#' Remove Objects via Pattern Match
#'
#' Sequentially removes objects from the `.GlobalEnv` that match
#' a regular expression pattern. You will be asked at each pattern match
#' whether to actually remove the matching object.
#'
#' @param pattern String. The regular expression to match.
#' @return `NULL`, invivibly.
#' @author Stu Field
#' @seealso \code{\link[base]{grep}}
#' @examples
#' hello <- 9
#' kitty <- rnorm(10)
#' fun <- "a"
#' kitten <- 101
#' ls()
#' rm_grep("kitt")
#' ls()
#' @export
rm_grep <- function(pattern) {
  if ( !is.character(pattern) ) {
     pattern <- deparse(substitute(pattern))
  }
  objs <- ls(.GlobalEnv, all.names = TRUE)
  xvec <- objs[grep(pattern, objs)]
  for ( i in xvec ) {
    do <- readline(sprintf("  Remove object? \"%s\" (y/n): ", i))
    if ( grepl("^y$", do, ignore.case = TRUE) ) {
      rm(list = i, envir = .GlobalEnv)
    } else {
      next
    }
  }
  invisible(NULL)
}
