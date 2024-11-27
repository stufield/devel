#' Package Objects
#'
#' The objects contained in the \pkg{stutils} package.
#' If known, the authorship of the object is attributed.
#'
#' @name objects
NULL

#' @describeIn objects
#' A sample `tibble` data frame for running examples and checking
#' functions that act on data frames. See `?tibble`.
#' @format **test-data**: An object of class `tbl_df`
#' (inherits from `tbl`, `data.frame`) with 40 rows and 12 columns and
#' defined as follows:
#' \tabular{ll}{
#'   Variable    \tab Definition \cr
#'   pid         \tab `sprintf("%02i", 1:40)`  \cr
#'   Pop         \tab `rep(head(LETTERS, 10), 4)` \cr
#'   Sample      \tab `factor(sample(c("small", "medium", "large"), 20))` \cr
#'   TimePoint   \tab `factor(rep(c("baseline", "6 mths", "12 mths", "24 mths"), each = 10))` \cr
#'   a           \tab `round(rnorm(40, mean = 25, sd = 3.5), 0)` \cr
#'   b           \tab `sample(0:6, 40, replace = TRUE)` \cr
#'   seq.1234.56 \tab `rnorm(40, mean = 25, sd = 3.5)` \cr
#'   seq.6969.42 \tab `rnorm(40, mean = 25, sd = 1.5)` \cr
#'   x           \tab `rnorm(40, mean = 25, sd = 3)` \cr
#'   y           \tab `round(runif(40, 10, 20), 0)` \cr
#'   z           \tab `round(rnrom(40, mean = 1, sd = 0.5), 3)` \cr
#'   Response    \tab `factor(sample(c("control", "disease"), 40, replace = TRUE)))` \cr
#' }
#' @source **test_data**: Stu Field
#' @examples
#' test_data
#' @usage test_data
"test_data"

#' @describeIn objects
#' Data set containing tree characteristic metric data,
#' that has been blinded and anonymized from its original source.
#' Used heavily in the `R` tutorial and for general data
#' frame usage (e.g. package testing). The format is `tibble` data
#" frame containing 20 cases and numerous random variables.
#' @references `tree_data`: originally from Sala et al., but heavily modified
#' to obscure its original source data values.
#' @format **tree_data**: An object of class `tbl_df` (inherits from `tbl`,
#' `data.frame`) with 20 rows and 10 columns.
#' @source **tree_data**: Sala et. al. \url{http}
#' @examples
#' summary(tree_data)
#' purrr::map_if(tree_data, is.numeric, mean)
#'
#' @usage tree_data
"tree_data"

#' @describeIn objects
#' Coefficient Interclass Correlation Data Example
#' from Sokal & Rohlf (Biometry; 3rd ed.), pages 210-214.
#' @format **Ri_data**: An object of class data.frame with 13 rows and 4 columns.
#' @source **Ri_data**: Sokal & Rohlf (Biometry; 3rd ed.), p. 210-214.
#' @examples
#' Ri_data
#'
#' @usage Ri_data
"Ri_data"

#' @describeIn objects
#' Infection data describing the infections
#' count data of an infectious disease
#' by geographic position. Includes the following variables:
#'   * CountyNo
#'   * CountyNo
#'   * CountyName
#'   * State
#'   * Fips
#'   * Site
#'   * Lat
#'   * Long
#'   * Population
#'   * Area
#'   * Density
#'   * Infected
#' @format **InfectionByCounty**: An object of class `tbl_df` (inherits
#' from `tbl`, `data.frame`) with 3071 rows and 11 columns.
#' @references `InfectionByCounty`: originally from Dylan George and
#' used for an exercise data set in the 2010 EEID R tutorial.
#' @source **InfectionByCounty**: Dylan George
#' @examples
#' InfectionByCounty
#'
#' @usage InfectionByCounty
"InfectionByCounty"
