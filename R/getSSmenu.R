
#' Get SomaSciences Menu Content
#'
#' Get a list of SeqIds corresponding to a particular 
#' SomaSciences content version. This function depends on 
#' \code{\link[SomaObjects]{SSmenu}} (\code{library(SomaObjects)}).
#'
#' @param menu.ver Character. Which SomaSciences menu version to select
#' @return A character vector of SeqIds of the SomaSciences menu of interest
#' @author Stu Field
#' @seealso \code{\link[SomaObjects]{SSmenu}}
#' @examples
#' getSSmenu("v1.1k_v01") %>% head(10)
#' @importFrom stringr str_glue str_squish
#' @export getSSmenu
getSSmenu <- function(menu.ver) {

  menu <- SomaObjects::SSmenu
  if ( !menu.ver %in% names(menu) ) {
    stringr::str_glue(
      "Cannot find specified menu version in
      names of SSmenu object: {menu.ver}."
      ) %>%
      stringr::str_squish() %>%
      stop(call. = FALSE)
  }

  atts <- attributes(menu)
  out  <- menu[[ menu.ver ]]
  attributes(out) <- list(Ver = menu.ver,
                          date = atts$date,
                          Author = atts$Author)
  return(out)

}

