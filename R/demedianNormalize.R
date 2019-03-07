#' Undo median normalization
#'
#' Reverse the steps of median normalization.
#' This involves dividing the appropriate analytes from
#' each dilution mix by the corresponding scale factor
#' that was used during median normalization.
#' Based on deNormalizeAdat, just adding sample selection
#'
#' @param adat A `soma.adat` object that has been median
#' normalized and contains median normalization scale factors.
#' @param do_field A field to use (in conjunction to `do_regexp`) to choose the
#' samples to be normalized (the rest keep their original values, and get scale
#' factors of 1.) `NULL` means to use all; the default is "SampleType"
#' (as for selecting only QCs and Samples in V4.)
#' @param do_regexp Character. A regexp to select samples from do_field to be
#' normalized. A value of `NULL` means to normalized all; the default value is
#' "QC|Sample" (with `ref_field = "SampleType"`) as for V4.
#' @param ... Additional arguments passed to \code{\link{getAptamerDilution}},
#' typically an "apt.data" object if `adat` has been modified and a
#' `stopifnot` is encountered.
#' @return A `soma.adat` object containing RFU values for each
#' analyte that corresponds to the non-normalized object.
#' @author Eduardo Tabacman, Michael R. Mehan, Stu Field
#' @seealso \code{\link{medianNormalize}}, \code{\link{deNormalizeAdat}}, \code{\link{getAptamerDilution}}
#' @examples
#' demedianNormalize(sample.adat)
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom rlang signal
#' @importFrom stringr str_remove_all
#' @importFrom SomaReadr cleanNames
#' @importFrom SomaNormalization getNormNames
#' @export demedianNormalize
demedianNormalize <- function(adat,
                            do_field = "SampleType",
                            do_regexp = "QC|Sample",
                            ...) {

  scale_names <- getNormNames(adat, drop.hyb = TRUE) %>%
    sort() %>% cleanNames()

  if ( length(scale_names) == 0  ) {
    rlang::signal("No median normalization scale factors detected in `adat`.",
                  "error")
  }

  mixes        <- getAptamerDilution(adat, drop.hyb = TRUE, ...)
  names(mixes) %<>% stringr::str_remove_all("[.]0$|%|^[.]*")
  names(mixes) <- paste0("NormScale.", names(mixes))
  mixes <- mixes[ sort(names(mixes)) ]

  if ( (length(mixes) != length(scale_names)) || (!all(names(mixes) == scale_names)) ) {
    print(scale_names)
    print(names(mixes))
    rlang::signal(
      "Mis-match between dilution mixes and available scale factors.", "error")
  }

  names(adat) %<>% cleanNames()

  # decide which samples get medNormalized
  if ( !(is.null(do_field) | is.null(do_regexp)) ) {
    do_samples <- grep(do_regexp, adat[[do_field]])
    if ( length(do_samples) == 0 ) {
      stop(
        stringr::str_glue(
          "No samples selected to normalize after `regex`!
          See `do_regexp = `{do_regexp} argument."
        ),
        call. = FALSE)
    }
  } else {       # use all
    do_samples   <- 1:nrow(adat)
  }

  for ( mix in scale_names ) {
    adat[do_samples, mixes[[mix]]] <- adat[do_samples, mixes[[mix]] ] / adat[[mix]][do_samples]
  }
  # set the corresponding scale factors to 1
  adat[do_samples,getNormNames(adat, drop.hyb = TRUE)] <- 1
  # if all are un-normalized, just erase all scale factors
  if (all(adat[,getNormNames(adat, drop.hyb = TRUE)] == 1)) adat[,getNormNames(adat, drop.hyb = TRUE)] <- NULL

  return(adat)
}
