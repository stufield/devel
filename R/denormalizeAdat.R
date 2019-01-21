# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
######################

#' Undo median normalization
#'
#' Reverse the steps of median normalization.
#' This involves dividing the appropriate analytes from 
#' each dilution mix by the corresponding scale factor 
#' that was used during median normalization.
#'
#' @param adat A `soma.adat` object that has been median 
#' normalized and contains median normalization scale factors.
#' @param ... Additional arguments passed to \code{\link{getAptamerDilution}},
#' typically an "apt.data" object if `adat` has been modified and a
#' `stopifnot` is encountered.
#' @return A `soma.adat` object containing RFU values for each 
#' analyte that corresponds to the Hybridization normalized object.
#' @author Michael R. Mehan, Stu Field
#' @seealso \code{\link{medianNormalize}}, \code{\link{getAptamerDilution}}
#' @examples
#' denormalizeAdat(sample.adat)
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom stringr str_remove_all
#' @export denormalizeAdat
denormalizeAdat <- function(adat, ...) {

  scale_names <- getNormNames(adat, drop.hyb = TRUE) %>%
    sort() %>% cleanNames()

  if ( length(scale_names) == 0  ) {
    stop("No median normalization scale factors detected in `adat`.",
         call. = FALSE)
  }

  mixes        <- getAptamerDilution(adat, drop.hyb = TRUE, ...)
  names(mixes) %<>% stringr::str_remove_all("[.]0$|%|^[.]*")
  names(mixes) <- paste0("NormScale.", names(mixes))
  mixes <- mixes[ sort(names(mixes)) ]

  if ( (length(mixes) != length(scale_names)) || (!all(names(mixes) == scale_names)) ) {
    print(scale_names)
    print(names(mixes))
    stop("Mis-match between dilution mixes and available scale factors.",
         call. = FALSE)
  }

  message("* Denormalizing ADAT ... {deparse(substitute(adat))}")
  names(adat) %<>% cleanNames()

  for ( mix in scale_names ) {
    adat[, mixes[[mix]]] <- adat[, mixes[[mix]] ] / adat[[mix]]
  }

  return(adat)

}
