#' Calculate CVs by Group
#'
#' Calculate CVs by a grouping variable, e.g. `SampleGroup`.
#'
#' @param data A `soma.adat` or `data.frame` object.
#' @param field Character. The group variable used to split the data.
#' @return Add return value here ...
#' @author Stu Field
#' @examples
#' calcCVbyField(sample.adat, "SampleGroup")
#' @importFrom purrr map_df map_dbl
#' @export
calcCVbyGroup <- function(data, group.var) {
  data %>%
    dplyr::select(getAptamers(.)) %>%    # no meta
    split(data[[group.var]]) %>%         # split in to dfs by var
    purrr::map_df(~ {                    # loop over dfs
      vapply(.x, function(.y) {sd(.y) / mean(.y)}, 0.1)   # loop over apts
    }) %>%
    dplyr::mutate(AptName = getAptamers(data)) %>% # add back apt column
    dplyr::select(AptName, dplyr::everything())    # move to 1st col
}
