#' Source Library of Functions
#'
#' Source a library of functions from a file
#' into a local environment and attach it
#'
#' @note This is the old `sf` function.
#' @author Stu Field
#' @seealso \code{\link[base]{sys.source}}, \code{\link{tryCatch}}, \code{\link[base]{attach}}
#' @examples
#' \dontrun{
#' sl("my.analysis.functions.R")
#' }
#' @export
sl <- function (file = "func_lib.R") {
  if ( !file.exists(file) ) {
    stop(paste("File doesn't exist:", file), call. = FALSE)
  }
  tryCatch(detach("lib", character.only = TRUE, force = TRUE, unload = TRUE),
           error = function(e) NULL)
  en <- new.env()
  sys.source(file, envir = en, keep.source = TRUE)
  attach(en, name = "lib", warn.conflicts = FALSE)
  invisible(NULL)
}
