#' Source Library of Functions
#'
#' Source a library of functions from a file
#'   into a local environment and attach it.
#'
#' @author Stu Field
#' @seealso \code{\link[base]{sys.source}}
#' @return NULL
#' @examples
#' sl("analysis-functions.R")
#' @export
sl <- function (file = "func_lib.R", name = "lib") {
  if ( !file.exists(file) ) {
    stop("File doesn't exist ... ", file, call. = FALSE)
  }
  tryCatch(
    detach(name, character.only = TRUE, force = TRUE, unload = TRUE),
    error = function(e) NULL
  )
  en <- new.env()
  sys.source(file, envir = en, keep.source = TRUE)
  attach(en, name = name, warn.conflicts = FALSE)
  invisible(NULL)
}
