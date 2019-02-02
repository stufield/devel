
#' List Env Functions & Objects (deprecate?)
#'
#' This function separately lists the functions and objects in global memory
#' (default).
#'
#' @param env Which environment is to be searched, defaults to \code{.GlobalEnv}.
#' @return A list of objects separated by object classes, and print a summary
#' of the numbers of each object by class.
#' @author Stu Field
#' @seealso \code{\link{ls}}
#' @examples
#' lss()
#' @export
lss <- function() {

  if ( file.exists("func.R") ) {
    sf()
    funcbox <- ls("funcbox")
  } else {
    funcbox <- NULL
  }
  glob.obj <- ls(.GlobalEnv)
  funs     <- getFuncs(env = .GlobalEnv)
  objects  <- setdiff(glob.obj, funs)

  if ( length(objects) == 0 && length(funs) == 0 ) {
    writeLines("*  Environment is totally empty ...")
  } else if ( length(objects) == 0 ) {
    cat(sprintf("*  Environment contains only functions ... %i\n", length(funs)))
  } else {
    mat <- objects[ sapply(objects, function(x) is.matrix(get(x, pos = 1))) ]
    tab <- objects[ sapply(objects, function(x) is.table(get(x, pos = 1))) ]
    mat <- setdiff(mat, tab)
    df  <- objects[ sapply(objects, function(x) is.data.frame(get(x, pos = 1))) ]
    adats <- objects[ sapply(objects, function(x)
                             inherits(get(x, pos = 1), "soma.adat")) ]
    df <- setdiff(df, adats)
    L  <- objects[ sapply(objects, function(x) is.list(get(x, pos = 1))) ]
    L  <- setdiff(L,c(df, adats))
    v  <- setdiff(objects, c(mat, df, L, tab, adats))
    pkg.funs <- unlist(sapply(grep("Soma", search(),
                                   value = TRUE,
                                   ignore.case = TRUE), function(.x) ls(.x)))
    obj_names <- c("pkg.funs", "funcbox", "global.funs", "list",
                   "adat", "data.frame", "matrix", "table", "vector")
    sum.objs <- list(pkg.funs, funcbox, funs, L, adats, df, mat, tab, v) %>%
      magrittr::set_names(obj_names)
    writeLines("*  Summary:")
    print(sapply(sum.objs, length))
    sum.objs[-1]
  }
}

