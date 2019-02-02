
#' Modify Aptamer Content
#'
#' Modify aptamer content of a data frame of aptamer data, typically subsetting
#' SOMAmer features (columns) to SomaSciences 1129 content.
#'
#' @param adat The adat to be modified.
#' @param menu.ver Numeric or Character. Which SomaSciences menu version to
#' use. The default is \code{menu.ver="v1.1k_v01"} which corresponds to the
#' 1129 menu. Should match one of the names of the \code{SSmenu} object.
#' @return An adat with modified aptamer content.
#' @author Stu Field
#' @seealso \code{\link{getSSmenu}}, \code{\link{getSeqIdMatches}}, \code{\link{SSmenu}}
#' @examples
#' dim(sample.adat)
#' x <- modifyAptContent(sample.adat)
#' dim(x)
#' names(SSmenu)
#' x <- modifyAptContent(sample.adat, menu.ver = "v1.1k_v02")  # v2 1124
#' dim(x)
#' @export modifyAptContent
modifyAptContent <- function(adat, menu.ver = "v1.1k_v01") {
  keep <- getSSmenu(menu.ver = menu.ver) %>%
    matchSeqIds(getAptamers(adat), order.by.x = FALSE)
  new <- dplyr::select_if(adat, names(adat) %in% c(getMeta(adat), keep))
  rownames(new) <- rownames(adat)
  class(new)    <- class(adat)
  return(new)
}

