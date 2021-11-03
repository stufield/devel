#' Calculate R-squared
#'
#' Returns the residual sum of squares of a model
#'
#' Details
#'
#' @param model A linear model of class "lm".
#' @return The R-squared of a model.
#' @author Stu Field
#' @export
calcRsquared <- function(model) {
  stopifnot(inherits(model, "lm"))
  stopifnot("model" %in% names(model))
  ss_res <-  sum(model$residuals^2)
  yi     <- model$model[, 1L]
  ss_tot <- sum((yi - mean(yi))^2)
  r2     <- 1 - ss_res / ss_tot
  if ( r2 < 0 ) {
    warning("Negative r^2 value indicates an inappropriate model",
         call. = FALSE)
  }
  r2
}
