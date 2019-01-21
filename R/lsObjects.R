# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
###############################
#         Function:   ls.env
###############################

#' List Object Size in an Environment
#'
#' Creates a data frame of all the objects in \code{default=.GlobalEnv} a
#' specified environment, and returns their object size (in KB), the class
#' of that object, and the dimension of the object (unless object is a function).
#'
#' @param env The environment to be searched.
#' @param units The units of the object size in the output data frame. Defaults
#' to "KB".
#' @return A tibble ordered first by the object size.
#' @author Stu Field
#' @seealso \code{\link[base]{ls}}, \code{\link{switch}},
#' \code{\link{get}}, \code{\link[pryr]{object_size}}
#' @examples
#'
#' lsObjects()
#' lsObjects("pkg.data")
#' lsObjects("SomaGlobals")
#'
#' @importFrom pryr object_size
#' @importFrom magrittr set_names
#' @importFrom purrr map_chr map_dbl
#' @importFrom tibble as.tibble
#' @export lsObjects
lsObjects <- function(env = .GlobalEnv, units = "MB") {

  units <- match.arg(units, c("b", "Kb", "Mb", "Gb",
                              "B", "KB", "MB", "GB"))
  #denom <- switch(units, KB = 1024, MB = 1024^2, GB = 1024^3, 1)
  denom <- switch(units, KB = 1000, MB = 1000^2, GB = 1000^3, 1)
  key   <- list("size") %>% magrittr::set_names(units)
  obj   <- ls(env)
  obj.list <- list()
  obj.list$object <- obj
  obj.list$size   <- purrr::map_dbl(obj, ~pryr::object_size(get(.x, env)))
  obj.list$dim  <- purrr::map_chr(obj, function(.x) {
                         o <- get(.x, env)
                         if ( inherits(o, "function") ) {
                            ""
                         } else if ( !is.null(dim(o)) ) {
                            paste(dim(o), collapse = " x ")
                         } else {
                            as.character(length(o))
                         }
       })
  obj.list$class <- purrr::map_chr(obj, function(.x)
                           paste(class(get(.x, env)), collapse = ", "))
  tibble::as.tibble(obj.list) %>%
    dplyr::mutate(size = size / denom %>% round(2L)) %>%
    dplyr::arrange(size) %>%
    dplyr::rename(!!!key)
}
