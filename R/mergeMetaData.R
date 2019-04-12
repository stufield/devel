
#' Merge Meta Data
#'
#' Merge additional meta data to an existing adat based on unique row sample
#' identifiers. This is a wrapper around the existing R function
#' \code{\link[dplyr]{left_join}}.
#'
#' @param adat The existing adat to which the meta data is to be merged.
#' @param meta Can be either:
#'   \enumerate{
#'   \item A data frame of additional meta data, with at
#'   least one common column name to match the samples OR
#'   \item A file name (and path) pointing to where to get a
#'   "*.csv" file containing meta data (this option uses a call
#'   to \code{\link[utils]{read.csv}}).
#'   }
#' @param fix.atts Logical. Should the attributes of the merged adat be fixed
#' by \code{\link{createChildAttributes}} or left broken?
#' @param ... Additional arguments passed to \code{\link[dplyr]{left_join}}.
#' @return An adat data frame with extra columns corresponding to the meta data
#' added and indexed to the appropriate sample IDs.
#' @author Stu Field
#' @seealso \code{\link[dplyr]{left_join}}, \code{\link[utils]{read.csv}}, \code{\link{createChildAttributes}}
#' @examples
#' set.seed(101)
#' new <- tibble::tibble(SampleUniqueID = sample(sample.adat$SampleUniqueID),
#'                       NewData = rnorm(20))
#' mergeMetaData(sample.adat, new)                         # use default 'by'
#' mergeMetaData(sample.adat, new, by = "SampleUniqueID")  # same
#' @importFrom utils read.csv
#' @importFrom purrr safely
#' @importFrom stringr str_glue
#' @importFrom rlang signal
#' @importFrom SomaReadr is.intact.attributes
#' @export mergeMetaData
mergeMetaData <- function(adat, meta, ..., fix.atts = FALSE)  {

  safe_exists <- purrr::safely(file.exists, otherwise = FALSE)
  if ( safe_exists(meta)$result ) {
    meta_df <- utils::read.csv(meta, stringsAsFactors = FALSE)
  } else if ( inherits(meta, c("data.frame", "tbl_df")) ) {
    meta_df <- meta
  } else {
    rlang::signal(
      stringr::str_glue(
        "The `meta =` argument must be one of: 
        1) a `data.frame` containing meta data
        2) a path to a CSV file containing meta data"
        ), "error")
  }

  adat_merge <- adat %>%
    dplyr::left_join(meta_df, ...) %>%       # left: only return rows from ADAT
    dplyr::select(getMeta(.), dplyr::everything())

  if ( nrow(adat) != nrow(adat_merge) ) {
    rlang::signal("New rows added during merg despite left_join()", "warning")
  }

  if ( !isTRUE(all.equal(data.matrix(adat[, getAptamers(adat)]),
                         data.matrix(adat_merge[, getAptamers(adat_merge)]),
                         check.attributes = FALSE)) )
    rlang::signal(
      "Feature RFU data mismatch during merging. Please check.",
      "error")

  if ( fix.atts ) {
    adat_merge %<>% createChildAttributes(adat, verbose = FALSE)
  }

  return(adat_merge)

}
