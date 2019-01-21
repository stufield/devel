# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
#################################
#         Function:   plotHemo
#################################

#' Plot Hemoglobin vs Haptoglobin
#'
#' Makes a scatter plot of all samples for Hemoglobin vs. Haptoglobin.
#'
#' @param adat Adat data set of SOMAmer data.
#' @param cutoff Where should the cutoff be placed for "high" Hemoglobin measurements.
#' @param filename Optional "filename" if \code{\link[SomaGlobals]{figure}}
#' is to be implemented.
#' @param legend.pos Character. Position of the legend.
#' @return A scatter plot and the ADAT rownames of the samples with a ratio
#' beyond the specified limit.
#' @author Stu Field
#' @seealso \code{\link[graphics]{plot}}, \code{\link[graphics]{legend}}
#' @examples
#' plotHemo(sample.adat, hb.hp.ratio = 0.2)
#' @export plotHemo
plotHemo <- function(adat, notes.field, grep.pattern = NULL,
                     cutoff = 30000, legend.pos = "bottomleft",
                     filename = NULL, hb.hp.ratio = 100, ...) {

  hb <- adat[[grep("4915\\.64", names(adat))]]
  hp <- adat[[grep("3054\\.3", names(adat))]]

  if ( missing(notes.field) ) {
    notes.field <- "ratio"
    adat$ratio <- sprintf("hb/hp %s %0.1f",
                          ifelse(hb / hp > hb.hp.ratio, ">=", "<"),
                          hb.hp.ratio)
    cutoff <- NULL
  }

  if ( !is.null(grep.pattern) ) {
    regx <- grep(grep.pattern, adat[[notes.field]],
                 ignore.case = TRUE, invert = TRUE)
    adat[ regx , notes.field ] = ""
  }

  cols <- classColor(adat[[notes.field]])
  figure(filename, width = 7, height = 7, scale = 1)
  on.exit(close_figure(filename))
  plot(hb, hp, pch = 21, col = 1, main = "Hemoglobin vs. Haptoglobin",
       ylab = "Haptoglobin (RFU)", xlab = "Hemoglobin (RFU)",
       bg = cols, log = "xy", ...)
  abline(v = cutoff, col = 1, lty = 2, lwd = 2)

  if ( !is.null(grep.pattern) )
    legend.vals <- c("\"\"", grep(grep.pattern, adat[, notes.field],
                                  ignore.case = TRUE, value = TRUE))
  else
    legend.vals <- names(table(adat[[notes.field]]))

  addLegend(legend.vals, col = 1, pt.bg = col.string, pch = 21, legend.pos)
  bool <- if (is.numeric(cutoff)) adat$HBA1.HBB.4915.64.2>=cutoff else hb / hp >= hb.hp.ratio
  rownames(adat)[ bool ]

}

