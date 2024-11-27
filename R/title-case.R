#' Capitalize Title Format
#'
#' Change case to capitalize first letter of 
#'   each word in a character string.
#'
#' @param x A character string in the form of a sentence 
#'   to be converted to "title case" (i.e. first letter capitalized).
#' @param strict Logical. Should first letter capitalization 
#'   be *strictly* applied? See example.
#'
#' @return A character string with "title case" conversion.
#' @author Stu Field
#'
#' @seealso [toupper()], [tolower()], [strsplit()]
#'
#' @examples
#' title_case("using AIC for model selection")
#' ## ->  [1] "Using AIC For Model Selection"
#' title_case(c("using AIC", "for MODEL selection"), strict = TRUE)
#' ## ->    [1] "Using Aic"  "For Model Selection"
#' ##                  ^^^        ^^^^^
#' ##                 "bad"       "good"
#' @export
title_case <- function(x, strict = FALSE) {
  sapply(strsplit(x, split = " "), function(.s) {
    ss <- substring(.s, 2)
    tt <- if ( strict ) tolower(ss) else ss
    paste(toupper(substring(.s, 1, 1)), tt, sep = "", collapse = " ")
    }, USE.NAMES = !is.null(names(x))
  )
}
