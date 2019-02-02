
#' Create Various Stat Test Tables
#'
#' Creates appropriate statistical output tables
#' for a given named list of data frames containing SOMAmer data.
#' Available tests are:
#' \describe{
#'   \item{`t`}{t.test}
#'   \item{`wilcox`}{mann-whitney}
#'   \item{`ks`}{ks.test}
#'   \item{`lr`}{log ratio}
#'   \item{`cor`}{pearson or spearman}
#'   \item{`kw`}{kw.test}
#' }
#' @param dat.list A named list of data frames containing SOMAmer data.
#' @param test Matched String. Which statistical test to perform.
#' @param which.paired which `dat.list` entries are paired analyses
#' (user must order properly)
#' @param ... Additional arguments passed through to the eventual "test fun",
#' e.g. `\link[stats]{t.test}`.
#' @return A named list of class "data.frame" tables, one for each entry of
#' \code{dat.list}.
#' @author Stu Field
#' @seealso \code{\link{get}}
#' @examples
#' age_dfs <- split(sample.adat, sample.adat$TimePoint) # split by Young/Old
#' sapply(age_dfs, function(x) table(x$SampleGroup))
#' ks <- createTestsList(age_dfs, test = "ks", response = "SampleGroup")
#' t <- createTestsList(age_dfs, test = "t", response = "SampleGroup")
#' @export createTestsList
createTestsList <- function(dat.list,
                            test = c("t", "wilcox", "ks", "cor", "lr", "kw", "mackwolfe"),
                            which.paired = numeric(0), ...) {

  if ( inherits(dat.list, "data.frame") ) {
    rlang::signal(
      stringr::str_glue(
        "The `dat.list =` argument is a single data.frame, but \\
        should be a LIST of data frames.
        Maybe you should try `calc.x()` directly?"
        ), "error")
  }

  if ( is.null(names(dat.list)) ) {
    rlang::signal("The `dat.list =` argument must be a *named* list.",
                  "error")
  }

  test <- match.arg(test)

  if ( !is.numeric(which.paired) || (length(which.paired) > 0 && max(which.paired) > length(dat.list)) ) {
    rlang::signal(
      stringr::str_glue(
        "Bad `which.paired =` argument. Should be numeric.
        Which `dat.list` entries are to be paired analyses?"
        ), "error")
  }

  if ( test == "mackwolfe" && any(!c("factor.order", "group.field") %in% names(list(...))) ) {
    rlang::signal(
      stringr::str_glue(
        "If performing a Mack-Wolfe test, you must pass both \\
        the `group.field =` and `factor.order =` arguments."
        ), "error")
  }

  which_unpaired <- setdiff(seq(length(dat.list)), which.paired)
  .fun           <- sprintf("calc.%s", test)
  args           <- list(...)

  unpaired_tests <- dat.list[which_unpaired] %>%
    purrr::map(function(.data) {
               args$training.data <- .data
               do.call(.fun, args)
         })

  if ( length(which.paired) > 0 ) {
    paired_tests <- dat.list[which.paired] %>%
      purrr::map(function(.data) {
                 args$training.data <- .data
                 args$paired <- TRUE
                 .fun(data, args)
        })
    ret <- c(unpaired_tests, paired_tests)[names(dat.list)]
  } else {
    ret <- unpaired_tests
  }

  if ( length(ret) == 1 ) {
    ret[[1]]
  } else {
    ret
  }

}

