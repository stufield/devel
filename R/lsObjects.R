#' List Object Size in an Environment
#'
#' Creates a data frame of all the objects in a specified environment, 
#' and returns their object size (in KB), the class
#' of that object, and the dimension of the object (unless object is a function).
#'
#' @param env The environment to be searched.
#' @param units The units of the object size in the output data frame.
#' @return A tibble ordered first by the object size.
#' @author Stu Field
#' @seealso \code{\link[base]{ls}}, \code{\link{switch}}
#' @examples
#' lsObjects()
#' @importFrom lobstr obj_size
#' @importFrom tibble as_tibble
#' @export
lsObjects <- function(env = .GlobalEnv, units = "MB") {

  units <- match.arg(units, c("b", "Kb", "Mb", "Gb",
                              "B", "KB", "MB", "GB"))
  #denom <- switch(units, KB = 1024, MB = 1024^2, GB = 1024^3, 1)
  denom <- switch(units, KB = 1000, MB = 1000^2, GB = 1000^3, 1)
  key   <- setNames(list("size"), units)
  obj   <- ls(env)
  obj_list <- list()
  obj_list$object <- obj
  obj_list$size <- vapply(obj, function(.x) as.numeric(lobstr::obj_size(get(.x, env))), 0)
  obj_list$dim  <- vapply(obj, function(.x) {
    o <- get(.x, env)
    if ( inherits(o, "function") ) {
      ""
    } else if ( !is.null(dim(o)) ) {
      paste(dim(o), collapse = " x ")
    } else {
      as.character(length(o))
    }
    }, "")
  obj_list$class <- vapply(obj, function(.x) paste(class(get(.x, env)), collapse = ", "), "a")
  obj_list |>
    tibble::as_tibble() |>
    dplyr::mutate(size = size / denom |> round(2L)) |>
    dplyr::arrange(size) |>
    dplyr::rename(!!!key)
}
