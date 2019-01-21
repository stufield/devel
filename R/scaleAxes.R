# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
################################
#  Function: ggplot aux functions
################################

#' Scale x-axis
#'
#' Term to add to ggplots to use with x_log axis (that get nice minor grid lines).
#' Unless you have a really high or low range, the default of -22 to +22 should be ok.
#'
#' @param epxmin Numeric. The exponent of the lower limit of labels
#' and gridlines we are displaying
#' @param epxmax Numeric. The exponent of the upper limit of labels
#' and gridlines we are displaying
#' @examples
#'
#' df.example <- data.frame(RFU = 10^seq(-5,5,by=0.1))
#' p <- ggplot(df.example, aes(x=RFU)) + stat_ecdf()
#' p + my_x_log10()
#'
#' @export
my_x_log10 <- function(expmin=-22, expmax=22) {
   scale_x_log10(breaks=c(t(outer(10^(expmin:expmax) , 1:9))),
                 labels=unlist(lapply(10^(expmin:expmax), function(l) c(l, rep("",8)))))
}

#' Scale y-axis
#'
#' Term to add to ggplots to use with y_log axis (that get nice minor grid lines)
#' Unless you have a really high or low range, the default of -22 to +22 should be ok.
#'
#' @param epxmin Numeric. The exponent of the lower limit of labels
#' and gridlines we are displaying
#' @param epxmax Numeric. The exponent of the upper limit of labels
#' and gridlines we are displaying
#' @examples
#'
#' df.example <- data.frame(RFU1 = 10^seq(-5,5,by=0.1), RFU2 = 10^seq(-5,5,by=0.1))
#' p <- ggplot(df.example, aes(x=RFU1, y=RFU2)) + geom_line()
#' p + my_y_log10()
#'
#' @export
my_y_log10 <- function(expmin=-22, expmax=22) {
  scale_y_log10(breaks=c(t(outer(10^(expmin:expmax) , 1:9))),
                labels=unlist(lapply(10^(expmin:expmax), function(l) c(l, rep("",8)))))
}



#' Color Hue for ggplot
#'
#' Function to get the (default) ggplot colors, in case one needs
#' to match some default plot
#'
#' @param n the number of colors to use
#' @return a vector of colors to pass into ggplot
#'
#' @export
gg_color_hue <- function(n) {
   hues <- seq(15, 375, length=n+1)
   hcl(h=hues, l=65, c=100)[1:n]
}

