# --------------------
# Revision Info
# --------------------
# $Id$
# $Author$
# $Date$
######################

#' Plot & Perform two-way ANOVA
#'
#' Plots an interaction plot (boxplot) of a two-way ANOVA analysis of two
#' factor variables on a response variable.
#'
#' @param dat The data frame from which the factor and Response variables are
#' contained.
#' @param y The response variable as a character string.
#' @param var A string of `length = 2` that contains the first and second
#' factor variables as they appear in the `colnames` of `dat`.
#' @param levels An optional argument for the ordering of the factors given as
#' a list; the first entry of the list for the first factor, etc.
#' @param shift A numeric value, usually between 0 and 0.5, altering the shift
#' in the boxes, which will be necessary depending on the factor levels
#' present.
#' @param do.int Logical. Should the p-value for the interaction term be plotted on the
#' interaction plot?
#' @param ... Additional arguments passed to `boxplot`.
#' @return A summary of the two-way ANOVA table.
#' @author Stu Field
#' @seealso \code{\link{aov}}, \code{\link{boxplot}}
#' @examples
#' plotTwoWayInteraction(test_data, y = "XYZZ.6969.4.7", var = c("Response", "TimePoint"))
#' plotTwoWayInteraction(test_data, y = "ABCD.1234.56.8", var = c("TimePoint", "Response"))
#' plotTwoWayInteraction(test_data, y = "z", var = c("Response", "TimePoint"),
#'                       levels = list(c("Disease", "Control"), NULL))
#' @importFrom graphics lines boxplot axis
#' @export plotTwoWayInteraction
plotTwoWayInteraction <- function(dat, y, var,
                                  levels = list(NULL, NULL),
                                  shift = 0.25, do.int = FALSE,
                                  y.lab = NULL, ...) {

  df <- dat[, c(var, y) ]

  if ( length(var) != 2 ) {
    stop(
      stringr::str_glue(
        "The `var =` agrument must be of length 2 indicating \\
        the column names of the 2 factors, e.g. c('Group', 'Time')
        Currently var = {vars}",
        vars = paste0(var, collapse = ", ")
        ),
      call. = FALSE)
  }

  if ( is.null(levels[[1]]) ) {
    f1 <- levels(factor(df[, var[1]]))
  } else {
    f1 <- levels[[1]]
  }

  if ( is.null(levels[[2]]) ) {
    f2 <- levels(factor(df[, var[2]]))
  } else  {
    f2 <- levels[[2]]
  }

  L1       <- length(f1)
  L2       <- length(f2)
  n.boxes  <- L1 * L2
  n.groups <- n.boxes / L1
  hi       <- seq(L1, n.boxes, L1)
  lo       <- hi - (L1 - 1)
  mid.box  <- sapply(1:n.groups, function(.x) lo[.x] + (hi[.x] - lo[.x]) / 2)
  box.pos  <- as.vector(sapply(1:n.groups, function(.x)
                               lo[.x]:hi[.x] + (mid.box[.x] - lo[.x]:hi[.x]) * shift
                               ))
  col.list <- col.string[1:L1]

  df.med <- subapply(dat, index = var, # This subapply call must to be replaced -> dplyr's version
                     .fun = mean, y = y,
                     longform = TRUE, outname = "means") 
  df.med[[ var[1] ]] <- factor(df.med[[ var[1] ]], levels = f1)      # gets ordering of levels right for lines
  df.med[[ var[2] ]] <- factor(df.med[[ var[2] ]], levels = f2)      # gets ordering of levels right for lines
  df.med <- df.med[ order(df.med[[ var[1] ]], df.med[[ var[2] ]]), ]

  if ( is.apt(y) ) {
    frmla <- as.formula(sprintf("log10(%s) ~ %s * %s", y, var[1], var[2]))
    df.med$means <- log10(df.med$means)
    if (is.null(y.lab))
      y.lab <- TeX("\\textit{log}$_{10}(RFU)$")
  } else {
    frmla <- as.formula(sprintf("%s ~ %s * %s", y, var[1], var[2]))
    if (is.null(y.lab)) y.lab <- y
  }

  graphics::boxplot(frmla,
                    data = df,
                    bg = "black",
                    outcol = col.list,
                    cex = 1, ylab = y.lab,
                    main = ifelse(is.apt(y), addGeneTitle(y), y),
                    pch = 21,
                    at = box.pos,
                    col = col.list,
                    xaxt = "n", ...)
  graphics::axis(1, at = mid.box, labels = f2, cex.axis = 1)

  sapply(f1, function(i) {
     med.vec <- df.med[ df.med[,var[1]] == i, "means" ]
     graphics::lines(mid.box, med.vec, type = "b", pch = 19,
                     cex = 1, lwd = 3, lty = 1, col = "black")
     graphics::lines(mid.box, med.vec, type = "b", pch = 21,
                     cex = 1.5, lwd = 1.5, lty = 1, bg = 1,
                     col = col.list[which(f1 == i)])
     })

  addLegend(values = f1, cols = col.list, pch = 15, bty = "n",
            title = sprintf("%s:",var[1]))

  if ( do.int ) {
    s_model <- summary(aov(frmla, data = dat))
    mtext(TeX(sprintf("$\\P^* = %0.3f$", s_model[[1]][3, ncol(s_model[[1]])])),
          side = 3, line = 0, at = 8, cex = 0.8)
    s_model
  }
}

