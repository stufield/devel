#' Scramble Class Variable in Data Frame
#'
#' Scramble the class variables in a given response variable column of a data
#' frame. The remaineder of the data frame is unchanged so that all
#' associations with the meta data and the response are broken.
#'
#' @param data The name of the data frame.
#' @param field Character. The field name of the variable column
#' containing the classes to be scrambled.
#' @param random.seed The random seed for the class scrambling (for reproducibility).
#' @return A vector of scrambled variables factors.
#' @author Stu Field
#' @seealso \code{\link{sample}}
#' @examples
#' scrambleClasses(sample.adat, "SampleGroup")
#' scrambleClasses(sample.adat, "TimePoint")
#' @export
scrambleClasses <- function(data, field="Response",
                            random.seed=sample(1000, 1)) {
   set.seed(random.seed)
   sample(data[[field]])
}

#' Permute Training Data
#'
#' @param tr.data Training data set to scramble. Must have
#' a "training.data" class object with a "Response" column.
#' @author Stu Field
#' @seealso \code{\link[base]{sample}}, \code{\link{createTrainingData}}
#' @examples
#'
#' @export
permute <- function(data, ...) UseMethod("permute")


