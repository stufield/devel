
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
#' lsObjects()
#' lsObjects("pkg.data")
#' lsObjects("SomaGlobals")
#' @importFrom lobstr obj_size
#' @importFrom purrr map_chr map_dbl set_names
#' @importFrom tibble as_tibble
#' @export lsObjects
lsObjects <- function(env = .GlobalEnv, units = "MB") {

  units <- match.arg(units, c("b", "Kb", "Mb", "Gb",
                              "B", "KB", "MB", "GB"))
  #denom <- switch(units, KB = 1024, MB = 1024^2, GB = 1024^3, 1)
  denom <- switch(units, KB = 1000, MB = 1000^2, GB = 1000^3, 1)
  key   <- list("size") %>% magrittr::set_names(units)
  obj   <- ls(env)
  obj_list <- list()
  obj_list$object <- obj
  obj_list$size   <- purrr::map_dbl(obj, ~ as.numeric(lobstr::obj_size(get(.x, env))))
  obj_list$dim    <- purrr::map_chr(obj, function(.x) {
                          o <- get(.x, env)
                          if ( inherits(o, "function") ) {
                             ""
                          } else if ( !is.null(dim(o)) ) {
                             paste(dim(o), collapse = " x ")
                          } else {
                             as.character(length(o))
                          }
    })
  obj_list$class <- purrr::map_chr(obj, 
                        ~ paste(class(get(.x, env)), collapse = ", "))
  obj_list %>%
    tibble::as_tibble() %>%
    dplyr::mutate(size = size / denom %>% round(2L)) %>%
    dplyr::arrange(size) %>%
    dplyr::rename(!!!key)
}
