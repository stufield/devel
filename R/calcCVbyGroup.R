#' Calculate CVs by Group
#'
#' Calculate CVs by a grouping variable, e.g. `SiteId`.
#'
#' @param data A `soma_adat` or `data.frame` object.
#' @param field Character. The group variable used to split the data.
#' @return A tibble with rows as the %CV for each group, and
#'   columns as analytes.
#' @author Stu Field
#' @examples
#' calcCVbyGroup(sim_test_data, "SiteId")
#' @importFrom dplyr bind_rows
#' @export
calcCVbyGroup <- function(data, group.var) {
  stopifnot(group.var %in% names(data))
  # split into dfs by var
  spl_df <- data[, getAnalytes(data)] |> split(data[[group.var]]) 
  # loop over dfs
  lapply(spl_df, function(.df) {
    # loop over apts
    vapply(.df, function(.x) {sd(.x) / mean(.x)}, double(1))
  }) |> dplyr::bind_rows(.id = "Group")
}
