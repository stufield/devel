
#' Create Concordance Table
#'
#' Description
#'
#' @param dat The data frame containing data.
#' @return A data frame.
#' @author Stu Field
#' @examples
#' createConcordTable(dat)
#' @export createConcordTable
createConcordTable <- function(dat) {
  iter <- dat$SampleId %>% unique() %>% sort()
  purrr::map_df(iter, function(id) {
                id.dat <- dat[ dat$SampleId==id, ]
                tmp_rn <- rownames(id.dat)
                apts <- getAptamers(id.dat)
                meds <- apply(id.dat[, apts], 2, median)
                tmp_df <- sapply(1:nrow(id.dat), function(.i)
                                 calcCCC(meds, as.numeric(id.dat[.i, apts]), do.log = TRUE) %>%
                                   unlist) %>%
                  t %>% data.frame
                rownames(tmp_df) <- tmp_rn
                tmp_df %<>% .[ order(.$rho.c), ]
                ci95 <- sprintf("(%0.3f, %0.3f)", tmp_df$ci95.lower, tmp_df$ci95.upper)
                tmp_df$ci95 <- ci95
                tmp_df$rho.c %<>% round(3)
                tmp_df[ c(1, nrow(tmp_df)), c("rho.c","ci95","p.value")]
   }) %>%
   magrittr::set_names(iter)
}
