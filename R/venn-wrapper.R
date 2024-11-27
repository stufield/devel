#' Venn Diagram Plotting Routine
#'
#' A wrapper function for plotting Venn diagrams comprising 2 -> 5
#' intersections.
#'
#' @inheritParams VennDiagram::venn.diagram
#' @inherit VennDiagram::venn.diagram details
#' @inherit VennDiagram::venn.diagram details
#' @param x A named list of vectors containing strings to match intersections.
#' @param ... Additional arguments passed to the underlying plotting routine
#' from [VennDiagram::venn.diagram()].
#' @param edge.col describe
#' @param num.cex describe
#' @param colors describe
#' @param alpha describe
#' @param label.col describe
#' @param fontfamily describe
#' @param margin describe
#' @param cat.fontface describe
#' @param cat.cex describe
#' @param cat.col describe
#' @param cat.fontfamily describe
#' @param rotation.degree Numeric (0-360). Rotation for the entire diagram.
#' @return A `grob` class object which can be sent to a plotting device or
#' printed to file with the `filename =` argument.
#' @note I stole this from the \pkg{VennDiagram} package.
#' @author Stu Field
#' @seealso [VennDiagram::venn.diagram()]
#' @references See the \pkg{VennDiagram} package.
#' @examples
#' int_list <- lapply(1:3, function(...) sample(LETTERS[1:10], 6)) %>%
#'   purrr::set_names(c("Larry", "Curly", "Mo"))
#' venn_wrapper(int_list, num.cex = seq(0.5, 2, length = 7), cat.cex = c(1, 1.5, 2),
#'              main = "Title Here", sub = "Subtitle")
#' @importFrom VennDiagram draw.triple.venn draw.pairwise.venn
#' @importFrom VennDiagram draw.single.venn draw.quad.venn
#' @importFrom VennDiagram draw.quintuple.venn add.title adjust.venn
#' @importFrom grid grid.draw gList textGrob
#' @importFrom grDevices dev.list dev.new dev.off pdf png tiff svg
#' @export
venn_wrapper <- function(x, main, filename = NULL, edge.col = "transparent", ...,
                         colors = seq(length(x)), num.cex = 1,
                         alpha = 0.25, label.col = "gray35",
                         fontfamily = "sans", main.fontface = "bold",
                         sub.fontface = "bold", main.col = 1, sub.col = "gray35",
                         main.cex = 3, sub.cex = 1, margin = 0.05,
                         cat.fontface = "bold", cat.cex = 2,
                         cat.col = "black", cat.fontfamily = "sans",
                         rotation.degree = 0) {

	if ( is.null(names(x)) ) {
		usethis::ui_stop(
		  "Must pass a *named* list ... please name {deparse(substitute(x))} and retry."
		)
  }
	.call      <- match.call(expand.dots = TRUE)
	.call$col  <- edge.col
	.call$cex  <- num.cex
	.call$fill <- colors
	if ( is.null(filename) )
      filename <- NA
	for ( i in setdiff(ls(), c("x", "edge.col", "num.cex", "colors")) ) {
		.call[[i]] <- get(i)
	}
	.call[[1L]] <- as.name("venn_default")
	rlang::eval_tidy(.call)
}


venn_default <- function(x, filename, height = 9, width = 9, scale = 1,
                         resolution = 500,
                         compression = "lzw", na = c("stop", "none", "remove"),
                         main = NULL, sub = NULL,
                         main.pos = c(0.5,1.05), main.fontface, sub.fontface,
                         main.fontfamily = "sans", main.col = "black", main.cex = 1,
                         main.just = c(0.5, 1), sub.pos = c(0.5, 1.05),
                         sub.fontfamily = "sans", sub.col = "black", sub.cex = 1,
                         sub.just = c(0.5, 1), category.names = names(x),
                         force.unique = TRUE, print.mode = "raw", sigdigs = 3,
                         direct.area = FALSE, area.vector = 0,
                         hyper.test = FALSE, total.population = NULL, ...) {

	if ( direct.area ) {
		if ( 1 == length(area.vector) ) {
			list.names <- category.names
			if ( is.null(list.names) ) {
            list.names <- ""
         }
			grob.list <- VennDiagram::draw.single.venn(area = area.vector[1],
																	 category = list.names,
																	 ind = FALSE, ...)
		}
		if (3 == length(area.vector)) {
			grob.list <- VennDiagram::draw.pairwise.venn(area1=area.vector[1],
																		area2=area.vector[2],
																		cross.area=area.vector[3],
																		category=category.names,
																		ind=FALSE, print.mode=print.mode,
																		sigdigs=sigdigs, ...)
		}
		if (7 == length(area.vector)) {
			grob.list <- VennDiagram::draw.triple.venn(area1=0, area2=0, area3=0, n12=0, n23=0, n13=0,
																	 n123=0, category=category.names, ind=FALSE,
																	 list.order=1:3, print.mode=print.mode, sigdigs=sigdigs,
																	 area.vector=area.vector, direct.area=TRUE, ...)
		}
		if (15 == length(area.vector)) {
			grob.list <- VennDiagram::draw.quad.venn(area1=0, area2=0, area3=0, area4=0, n12=0, n13=0,
																  n14=0, n23=0, n24=0, n34=0, n123=0,
																  n124=0, n134=0, n234=0, n1234=0, category=category.names,
																  ind=FALSE, print.mode=print.mode, sigdigs=sigdigs,
																  area.vector=area.vector, direct.area=TRUE, ...)
		}
		if (31 == length(area.vector)) {
			grob.list <- VennDiagram::draw.quintuple.venn(area1=0, area2=0, area3=0, area4=0, area5=0, n12=0,
																		 n13=0, n14=0, n15=0, n23=0, n24=0,
																		 n25=0, n34=0, n35=0, n45=0, n123=0,
																		 n124=0, n125=0, n134=0, n135=0, n145=0,
																		 n234=0, n235=0, n245=0, n345=0, n1234=0,
																		 n1235=0, n1245=0, n1345=0, n2345=0, n12345=0,
																		 category=category.names, ind=FALSE,
																		 print.mode=print.mode, sigdigs=sigdigs,
																		 area.vector=area.vector,
																		 direct.area=TRUE, ...)
		}
	} else {

		na <- match.arg(na)

		if ( force.unique ) {
			for ( i in 1:length(x) ) {
				x[[i]] <- unique(x[[i]])
			}
		}

		if ( "none" == na ) {
			x <- x
		} else if ( "stop" == na ) {
			for ( i in 1:length(x) ) {
				if ( any(is.na(x[[i]])) )
					stop("NAs in lists", call.=FALSE)
			}
		} else if ( "remove" == na ) {
			for ( i in 1:length(x) ) {
				x[[i]] <- x[[i]][!is.na(x[[i]])]
			}
		} else {
			stop("Invalid [na] option: valid options are \"none\", \"stop\", and \"remove\"", call.=FALSE)
		}

		if (0 == length(x) | length(x) > 5) {
			stop("Incorrect number of elements.", call.=FALSE)
		}

		if (1 == length(x)) {
			list.names <- category.names
			if (is.null(list.names)) { list.names <- "" }
			grob.list <- VennDiagram::draw.single.venn(area=length(x[[1]]), category=list.names, ind=FALSE, ...)
		}
		else if (2 == length(x)) {
			grob.list <- VennDiagram::draw.pairwise.venn(area1=length(x[[1]]), area2=length(x[[2]]),
																		cross.area=length(intersect(x[[1]], x[[2]])),
																		category=category.names, ind=FALSE,
																		print.mode=print.mode, sigdigs=sigdigs, ...)
		}
		else if (3 == length(x)) {
			A <- x[[1]]
			B <- x[[2]]
			C <- x[[3]]
			list.names <- category.names
			nab <- intersect(A, B)
			nbc <- intersect(B, C)
			nac <- intersect(A, C)
			nabc <- intersect(nab, C)
			grob.list <- VennDiagram::draw.triple.venn(area1=length(A),
																	 area2=length(B), area3=length(C), n12=length(nab),
																	 n23=length(nbc), n13=length(nac), n123=length(nabc),
																	 category=list.names, ind=FALSE, list.order=1:3,
																	 print.mode=print.mode, sigdigs=sigdigs, ...)
		}
		else if (4 == length(x)) {
			A <- x[[1]]
			B <- x[[2]]
			C <- x[[3]]
			D <- x[[4]]
			list.names <- category.names
			n12 <- intersect(A, B)
		 	n13 <- intersect(A, C)
			n14 <- intersect(A, D)
			n23 <- intersect(B, C)
			n24 <- intersect(B, D)
			n34 <- intersect(C, D)
			n123 <- intersect(n12, C)
			n124 <- intersect(n12, D)
			n134 <- intersect(n13, D)
			n234 <- intersect(n23, D)
			n1234 <- intersect(n123, D)
			grob.list <- VennDiagram::draw.quad.venn(area1=length(A),
																  area2=length(B), area3=length(C), area4=length(D),
																  n12=length(n12), n13=length(n13), n14=length(n14),
																  n23=length(n23), n24=length(n24), n34=length(n34),
																  n123=length(n123), n124=length(n124), n134=length(n134),
																  n234=length(n234), n1234=length(n1234), category=list.names,
																  ind=FALSE, print.mode=print.mode, sigdigs=sigdigs, ...)
		}
		else if (5 == length(x)) {
			A <- x[[1]]
			B <- x[[2]]
			C <- x[[3]]
			D <- x[[4]]
			E <- x[[5]]
			list.names <- category.names
			n12 <- intersect(A, B)
			n13 <- intersect(A, C)
			n14 <- intersect(A, D)
			n15 <- intersect(A, E)
			n23 <- intersect(B, C)
			n24 <- intersect(B, D)
			n25 <- intersect(B, E)
			n34 <- intersect(C, D)
			n35 <- intersect(C, E)
			n45 <- intersect(D, E)
			n123 <- intersect(n12, C)
			n124 <- intersect(n12, D)
			n125 <- intersect(n12, E)
			n134 <- intersect(n13, D)
			n135 <- intersect(n13, E)
			n145 <- intersect(n14, E)
			n234 <- intersect(n23, D)
			n235 <- intersect(n23, E)
			n245 <- intersect(n24, E)
			n345 <- intersect(n34, E)
			n1234 <- intersect(n123, D)
			n1235 <- intersect(n123, E)
			n1245 <- intersect(n124, E)
			n1345 <- intersect(n134, E)
			n2345 <- intersect(n234, E)
			n12345 <- intersect(n1234, E)
			grob.list <- VennDiagram::draw.quintuple.venn(area1=length(A),
																		 area2=length(B),
																		 area3=length(C),
																		 area4=length(D),
																		 area5=length(E),
																		 n12=length(n12), n13=length(n13),
																		 n14=length(n14), n15=length(n15),
																		 n23=length(n23), n24=length(n24),
																		 n25=length(n25), n34=length(n34),
																		 n35=length(n35), n45=length(n45),
																		 n123=length(n123), n124=length(n124),
																		 n125=length(n125), n134=length(n134),
																		 n135=length(n135), n145=length(n145),
																		 n234=length(n234), n235=length(n235),
																		 n245=length(n245), n345=length(n345),
																		 n1234=length(n1234), n1235=length(n1235),
																		 n1245=length(n1245), n1345=length(n1345),
																		 n2345=length(n2345), n12345=length(n12345),
																		 category=list.names, ind=FALSE,
																		 print.mode=print.mode, sigdigs=sigdigs, ...)
		}
		else {
			stop("Invalid size of input object ... must be 1-5 in length", call.=FALSE)
		}
	}

	if ( !is.null(sub) ) {
		grob.list <- add.title(gList=grob.list, x=sub, pos=sub.pos,
									  fontface=sub.fontface, fontfamily=sub.fontfamily,
									  col=sub.col, cex=sub.cex)
	}

	if ( !is.null(main) ) {
		grob.list <- add.title(gList=grob.list, x=main, pos=main.pos,
									  fontface=main.fontface, fontfamily=main.fontfamily,
									  col=main.col, cex=main.cex)
	}

	# file print
	if ( !is.na(filename) ) {

		imagetype <-  getFileExt(filename)
		current.type <- getOption("bitmapType")

		if ( length(grep("Darwin", Sys.info()["sysname"])) ) {
			options(bitmapType="quartz")
      } else {
			options(bitmapType="cairo")
      }

		height <- height * scale
		width  <- width * scale

		if ( "tiff" == imagetype ) {
			tiff(filename=filename, height=height, width=width,
				  units="in", res=resolution, compression=compression)
		} else if ( "pdf" == imagetype ) {
			pdf(file=filename, height=height, width=width, useDingbats=FALSE, title=basename(filename))
		} else if ( "png" == imagetype ) {
			png(filename=filename, height=height, width=width, units="in", res=resolution)
		} else if ( "svg" == imagetype ) {
			svg(filename=filename, height=height, width=width)
		} else {
			stop("Unknown imagetype in file extension ... please check filename argument", call.=FALSE)
		}
		on.exit(dev.off())
		on.exit(options(bitmapType = current.type), add = TRUE)
	}
	grid::grid.draw(grob.list)
	invisible(grob.list)
}
