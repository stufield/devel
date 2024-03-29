#' Source Files from Directory
#'
#' Evaluates and parses all `*.R` files and their contained functions
#' into a specified environment and attaches that environment
#' to the search path.
#'
#' @param path Location of the directory containing `*.R` files.
#' @param envir.name Character. A name for the environment to
#' place the function definitions. This name will be used
#' when attaching the environment to the search path.
#' @note The environment is attached in the *final* position
#' in the search path.
#' @author Stu Field
#' @seealso \code{\link{sys.source}}, \code{\link[fs]{dir_ls}}, \code{\link{attach}}
#' @examples
#' \dontrun{
#' dir <- paste0(Sys.getenv("R_SOMA_DEV"), "sandbox", "/R")
#' Sys.sourceDir(dir, envir.name = "sandbox")
#' }
#' @importFrom fs path_real dir_ls
#' @export
Sys.sourceDir <- function(path, envir.name = "e") {
	files <- fs::dir_ls(path, regexp = "[.][Rr]$") %>% fs::path_real()
	if ( length(files) == 0 ) {
    stop(paste0("No *.R scripts present in: ", path), call. = FALSE)
  }
	if ( envir.name %in% search() ) {
		detach(envir.name, unload = TRUE, force = TRUE, character.only = TRUE)
  }
	en <- new.env()
	lapply(files, sys.source, envir = en, keep.source = TRUE)
	attach(en, name = envir.name, warn.conflicts = FALSE, pos = length(search()))
}
