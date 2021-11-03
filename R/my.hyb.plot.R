#' Plot Hyb Scale Factors & CDF
#'
#' A function that plots the Hybridization Scale Factors from Hyb Normalized
#' data. Plots both the CDF and barplot of the scale factors.
#'
#' @param dat A loaded data frame of aptamer data (ADAT).
#' @param title A string indicating the data title for the ADAT.
#' @return A CDF plot and a barplot.
#' @note %% ~~further notes~~
#' @author Stu Field, Darryl Perry
#' @seealso \code{\link[SomaGlobals]{plotCDF}}
#' @examples
#'
#'
#' @export
my.hyb.plot <- function(dat, title = "NULL") {
   opar <- par(par.def)
   on.exit(par(opar))
   dat <- dat[ order(dat$PlateId, dat$SlideId, dat$Subarray), getMeta(dat) ]

   layout(matrix(c(1,1,1,0,2,0,3,3,3), ncol=3, byrow=TRUE), TRUE)

   hybname <- grep("^Hyb.Scale|^HybControl", names(dat), value=TRUE)
   hyb.vals <- dat[[hybname]]

   y <- barplot(hyb.vals, xaxs="i", col=NA, border=NA, ylim=c(0,2.5), xpd=FALSE,
                ylab="Hyb Scale Factor",
                main="Plate-wide Hybridization Scale Factors by Slide",
                xlab="Ordered by Slide x Subarray")

   subarrays <- lapply(unname(split(dat$SlideId, dat$PlateId)), table)
   y <- y[-length(y)] + diff(y) / 2
   abline(v=y[cumsum(unlist(subarrays))], col="darkred", lty=2); box()

   barplot(hyb.vals, col=col.string[dat$Subarray], xaxs="i", xpd=FALSE, add=TRUE)
   par(pty="s")
   plotCDF(log2(hyb.vals), col="navy", lwd=2, xlim=c(-2.5, 2.5),
           xlab="log2(Hyb Scale Factors)", plot.fit=TRUE, do.log=FALSE,
           main="CDF of Hybridization Scale Factors")

   addCDFquantiles(log2(hyb.vals), p=0.5, col="darkred")
   abline(v=log2(c(0.4,2.5)), lty=2, col=8)

   mtext(sprintf("Data: %s\nSamples: %i", title, length(hyb.vals)),
         side=4, line=9, las=2, cex=2)

   par(pty="m")
   myBoxplot(dat, y=hybname, f1="SlideId", jitterAxis=TRUE, x.lab="",
              cex.axis=0.9, label.levs=2, box.order=names(unlist(subarrays)), pw.col=col.string[dat$Subarray],
              col="gray88", ylim=c(0,max(2.5,hyb.vals))
              )
   abline(h=1.0, col=8, lty=2)
   plates <- cumsum(sapply(subarrays,length))
   abline(v=plates[-length(plates)]+0.5, col="darkred", lty=2)
   invisible(dat)
}
