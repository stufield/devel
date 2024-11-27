#' Determine Time from Seconds (& vice versa)
#'
#' [time2seconds()] and [seconds2time()] determine the
#'   time (format="hh:mm:ss.ss") from the value in seconds or vice versa.
#'   Two digit hour precision optional.
#'
#' @rdname time
#'
#' @param x `character(1)` for [seconds2time()], `numeric(1)` for
#'   [time2seconds()]. Use format `hh:mm:ss.ss` when converting
#'   to seconds. Use 2 decimal point precision to convert to time.
#'
#' @return Either number of seconds (`numeric(1)`) or the time format
#'   (`character(1)`) as `hh:mm:ss.ss`.
#'
#' @author Stu Field
#'
#' @seealso [strsplit()], [grep()]
#' @examples
#' seconds2time(159.72)
#'
#' @export
seconds2time <- function(x) {
  hours <- x %/% 3600
  mins  <- (x %% 3600) %/% 60
  secs  <- ((x %% 60) %% 60) %% 60
  decimals <- gsub(".*[0-9][.]", "", sprintf("%0.2f", secs))
  sprintf("%01d:%02d:%02d.%s", hours, mins, round(secs, 0L), decimals)
}


#' Determine Seconds from Time
#'
#' @rdname time
#'
#' @examples
#' time2seconds("3:44:12.04")
#' time2seconds("15:44:12.04")
#' @export
time2seconds <- function(x) {
  format_check <- grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}[.][0-9]{1,2}$", x)
  if ( !format_check ) {
    stop(
      "Incorrect time format ... check time format is `hh:mm:ss.ss`",
      call. = FALSE
    )
  }
  char_split <- strsplit(x, split = ":", fixed = TRUE)[[1L]]
  convert    <- as.numeric(char_split)
  hr  <- convert[1L]
  min <- convert[2L]
  sec <- convert[3L]
  return(hr * 60 + min * 60 + sec)
}
