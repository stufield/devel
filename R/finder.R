#' Find Function or Object
#'
#' Function using fuzzy regular expression matching to determine
#' the location within the SomaPackages of a particular
#' funtion (or object)
#'
#' Note that *if* a matched function lives inside a file under
#' a different name, the editing mechanism will not be able to find
#' the appropriate file and open it. This can happen when numerous
#' closely related functions have been bundled inside the same parent
#' file. In such cases, the user is directed to the appropriate
#' directory where a manual search from there should be trivial.
#' See example below with \code{getInput}.
#'
#' @param what Character. Pattern to recognize
#' @param edit Logical. Should the function be opened in an editor after it is found?
#' @param pos Alternative output, should the position in the search path be returned?
#' @return A list of the packages which matched the pattern and the corresponding
#' function names, or alternatively their positions in the search path.
#' @author Stu Field
#' @seealso \code{\link[base]{grep}}, \code{\link[utils]{apropos}}
#' @examples
#' finder("mean")
#' finder("calc")
#' \dontrun{ finder("calc", edit = TRUE) }  # edit file; will require user input
#'
#' # Bundled function; won't be able to find file
#' finder("getInput", edit = TRUE)
#' @importFrom utils apropos file.edit
#' @importFrom stringr str_replace_all
#' @importFrom purrr set_names
#' @export
finder <- function(what, edit = FALSE, pos = FALSE) {

  ret <- utils::apropos(what, where = TRUE)        # get fuzzy matches
  idx <- names(ret) %>% unique() %>% as.numeric()
  ret <- split(x = unname(ret),
               f = factor(as.numeric(names(ret)))) # combine and summarize
  names(ret) <- search()[ idx ]                    # get which envir it lives

  if ( length(ret) == 0 ) {
    message("* Pattern ", what, " not found")
    return(character(0L))
  } else {
    names(ret) %<>% stringr::str_replace_all(":", "_")
    where <- which(search() %in% names(ret))
    if ( pos )
      return(where %>% purrr::set_names(names(ret)))
  }

  if ( edit ) {

    if ( length(ret) > 1 ) {
      cat("Which Package:\n")
      pkg_idx <- getInput(ret)
      ret     <- ret[ pkg_idx ]
    }

    if ( !grepl("^Soma|^devel$", names(ret)) )
      stop("You should not edit functions outside SomaPackages.",
           call. = FALSE)

    if ( length(ret[[1]]) > 1 ) {
      cat("Which function:\n")
      idx  <- getInput(ret[[1]])
      stem <- ret[[1]][idx]
    } else {
      stem <- ret[[1]][1]
    }

    file <- paste0(Sys.getenv("R_SOMA_DEV"), names(ret), "/R/", stem, ".R")

    if ( file.exists(file) ) {
      file.edit(file)
    } else {
      cat("* File does not exist:", file, "\n")
      cat("* This function is likely bundled within another related file ...\n")
      cat("* Please do a little digging in",
          paste0(names(ret), "/R"), "... a grep may be necessary ...\n")
    }

  } else {
    return(ret)
  }
}



#' Get Input File Index
#'
#' Internal function to finder(), when multiple matches exist.
#' Used to control user input when selecting from match alternatives.
#'
#' @param x the internal result of an `apropos` call. It is
#' typically the, or part of the, *ret* object created above.
#' @author Stu Field
#' @noRd
getInput <- function(x) {

  if ( inherits(x, "list") ) {
    sapply(seq_along(x), function(.i)
           cat(sprintf("%s. %s: %s\n",
                       .i, names(x)[.i],
                       paste(x[[.i]], collapse = ", "))))
  } else {
    sapply(seq_along(x), function(.i)
           cat(sprintf("%s. %s\n", .i, x[.i])))
  }

  out <- as.numeric(readline("*  Enter #: "))

  if ( !out%in%seq_along(x) ) {
    stop(
      paste0("Invalid entry. Please enter number: ",
             paste(seq_along(x), collapse = ", ")), call. = FALSE)
  }
  out
}

