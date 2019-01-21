# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
################################
#         Function:   rm.grep
################################

#' Remove Objects via Pattern Match
#'
#' Sequentially removes objects from the \code{".GlobalEnv"} that match
#' a regular expression pattern. You will be asked at each pattern match
#' whether to actually remove the matching object.
#'
#' @param pattern String. The regular expression to match.
#' @return No return value
#' @author Stu Field
#' @seealso \code{\link[base]{grep}}
#' @examples
#'
#' hello = 9
#' kitty = rnorm(10)
#' fun = "a"
#' kitten = 101
#' ls()
#' rm.grep("kitt")
#' ls()
#'
#' @export rm.grep
rm.grep <- function(pattern) {
   if ( !is.character(pattern) )
      pattern <- deparse(substitute(pattern))
   objs <- ls(.GlobalEnv, all.names=TRUE)
   xvec <- objs[grep(pattern, objs)]
   for ( i in xvec ) {
      do <- readline(sprintf("  Remove object? \"%s\" (y/n): ", i))
      if ( grepl("^y$", do, ignore.case=TRUE) )
         rm(list=i, envir=.GlobalEnv)
      else
         next
   }
}
#### ---- END FUNCTION ---- ####

# ---- created on: 2014-05-07 13:27:00
