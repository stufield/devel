
#' Calculate correlated clusters
#'
#' Calculate clusters given a data frame and 
#' aptamers (cols) and a threshold value.
#'
#' @param data A data frame containing aptamer data to 
#' be used in producing a heatmap.
#' @param cor.method Character. The correlation method to be used.
#' @param cluster.method The clustering method to be used buy
#' \code{\link[seriation]{seriate}}.
#' @param zero.nonsig Should non-significant correlation coefficients be
#' zeroed?
#' @param abs Should absolute values of the correlations be used?
#' @param thresh The threshold used to produce the adjacency matrix.
#' @param ... Additional arguments passed to the S3 plot method.
#' @return A heatmap with clusters based on the correlation matrix.
#' @author Stu Field
#' @seealso \code{\link{seriation}}, \code{\link{cor}}
#' @examples
#' \dontrun{
#' cluster <- clusterCorHeatmaps(M, thresh = 0.3)
#' }
#' @importFrom seriation seriate get_order
#' @importFrom stats cor qt
#' @export clusterCorHeatmaps
clusterCorHeatmaps <- function(data, cor.method = "spearman",
                               cluster.method = "OLO",
                               zero.nonsig = FALSE, abs = TRUE,
                               thresh = NULL, ...) {

  if ( missing(data) ) {
    stop("Must provide a data matrix or data frame!",
         call. = FALSE)
  }

  if ( any(!apply(data, 2, is.numeric))) {
    stop(
      stringr::str_glue(
        "All entries *must* be numeric to correctly \\
        produce a correlation matrix. Is there meta data?
        Perhaps pre-process with `stripMeta()`?"
        ),
      call. = FALSE)
  }

  if ( isSymmetric(data.matrix(data)) ) {
    message("* Assuming already a correlation matrix ...")
    CorMatrix <- data
    title     <- "Correlation Heatmap Image"
  } else {
    if ( is.null(colnames(data)) ) {
      stop("Data frame or correlation matrix must have column names.",
           call. = FALSE)
    }
    title <- stringr::str_glue(
               "Correlation Heatmap | {cor.method} | (all values)"
               )

    if ( inherits(data, "data.frame") ) {
      data %<>% data.matrix(rownames.force = FALSE)
    }

    CorMatrix <- stats::cor(data, method = cor.method)  # calc correlation matrix
  }

  shortnames          <- colnames(CorMatrix) %>% removeSeqId()
  dimnames(CorMatrix) <- list(shortnames, shortnames)

  if ( abs ) {
    CorMatrix <- abs(CorMatrix)   # absolute values of cor coeffs
  }

  my_order  <- seriation::seriate(as.dist(1 - CorMatrix),
                                  method = cluster.method) %>%
    seriation::get_order()
  CorMatrix <- CorMatrix[my_order, my_order]
  alpha2    <- 0.05 / ncol(CorMatrix)
  t.crit    <- stats::qt(p = 1 - (alpha2 / 2), df = nrow(data) - 2)        # calc crit t-value
  cor.crit  <- t.crit / sqrt(t.crit^2 + nrow(data) - 2)   # calc crit Rho based on t-value

  if ( zero.nonsig ) {
    CritCorMatrix <- CorMatrix
    CritCorMatrix[CritCorMatrix < cor.crit] <- 0 # set correlations less than crit=0
    title       <- sprintf("Correlation Heatmap | %s > %0.3f", cor.method, cor.crit)
    plot_matrix <- CritCorMatrix
  } else {
    plot_matrix <- CorMatrix
  }

  if ( is.null(thresh) ) {
    thresh <- cor.crit
  }

  adjMat <- plot_matrix           # set up adjacency matrix
  adjMat[adjMat < thresh]  <- 0
  adjMat[adjMat >= thresh] <- 1
  diag(adjMat) <- 0

  clusters     <- connected.components(adjMat)
  cluster.tab  <- table(clusters)
  clusters.out <- lapply(1:length(cluster.tab), function(n)
                         colnames(adjMat)[clusters == n]) %>%
    magrittr::set_names(sprintf("cluster%i", 1:length(cluster.tab)))

  list(image.matrix = plot_matrix,
       adjMat       = adjMat,
       title        = title,
       clustervec   = clusters,
       cluster.tab  = cluster.tab,
       clusters.out = clusters.out,
       thresh       = thresh,
       SigCorsOnly  = zero.nonsig) %>%
    addClass("heatmap.clusters") %>%
    invisible()
}



#' Plot Heatmap Cluster Object
#'
#' S3 plot method for objects of class `"heatmap.clusters"`.
#'
#' @rdname heatmap.clusters
#' @param x A \code{"heatmap.clusters"} class object.
#' @param color.scheme Describe \code{color.fun} here~~
#' @param plot.cluster Integer. Plot a specific cluster indexed by
#' the clusters in \code{x$clusters.out}.
#' @param plot.xaxis Describe \code{plot.xaxis} here~~
#' @param plot.yaxis Describe \code{plot.yaxis} here~~
#' @param graph.layout Describe \code{graph.layout} here~~
#' @param rgl Logical. Should an \code{rgl} plot be generated?
#' @param vertex.label.color Color of label on vertices
#' @param vertex.label.dist Location of label on vertices
#' @return A heatmap cluster plot.
#' @author Stu Field
#' @seealso \code{\link{heatmap.clusters}}
#' @examples
#' \dontrun{
#' plot(cluster)
#' plot(cluster, plot.cluster = 2)
#' }
#' @importFrom igraph rglplot graph.adjacency
#' @importFrom rgl rgl.clear
#' @importFrom graphics image layout axis mtext
#' @method plot heatmap.clusters
#' @export
plot.heatmap.clusters <- function(x, color.scheme = heat.colors(100),
                                  plot.cluster = NULL,
                                  plot.xaxis = FALSE,
                                  plot.yaxis = FALSE,
                                  graph.layout = layout.kamada.kawai,
                                  rgl = FALSE,
                                  vertex.label.color = "white",
                                  vertex.label.dist = 0) {

  layout.m <- matrix(rep(c(1, 3), c(9, 12)), ncol = 7)
  layout.m[1,4] <- 2
  layout(layout.m)

  dims <- dim(x$image.matrix)

  graphics::image(seq(dims[2]), seq(dims[1]), x$image.matrix,
                  xaxt = "n", yaxt = "n", zlim = 0:1,
                  col = color.scheme, ylab = "", xlab = "")

  mtext(x$title, font = 4, cex = 1.25, col = "darkred")

  if ( plot.xaxis ) {
    axis(1, at = seq(dims[2]), labels = colnames(x$image.matrix), las = 2)
  }

  if ( plot.yaxis ) {
    axis(2, at = seq(dims[2]), labels = colnames(x$image.matrix), las = 2)
  }

  #tmp_seq <- seq(min(x$image.matrix), max(x$image.matrix), length = 250)
  tmp_seq <- seq(0, 1, length = 250)
  graphics::image(t(as.matrix(tmp_seq)), col = color.scheme,
                  xaxt = "n", main = "Color Key", zlim = c(0, 1))

  node.colors <- unlist(soma.colors)[x$clustervec]

  if ( !is.null(plot.cluster) ) {
    x$adjMat <- x$adjMat[x$clusters.out[[plot.cluster]],x$clusters.out[[plot.cluster]]]
    x$image.matrix <- x$image.matrix[x$clusters.out[[plot.cluster]],
                                     x$clusters.out[[plot.cluster]]]
    node.colors <- unlist(soma.colors)[plot.cluster]
  }

  igraph_obj <- igraph::graph.adjacency(x$adjMat,
                                        mode = "undirected",
                                        weighted = TRUE,
                                        diag = FALSE)
  temp.adjMat <- x$adjMat
  temp.adjMat[ upper.tri(temp.adjMat) ] <- 0

  # adjust color range to match original image range
  color_map_vals <- c(0, 1, x$image.matrix[ temp.adjMat > 0 ])
  color_map      <- SomaPCA::mapColor(color_map_vals,
                                      color.scheme)[3:length(color_map_vals)]
  widths         <- SomaPCA::mapColor(color_map_vals[3:length(color_map_vals)],
                                      seq(5))

  if ( rgl ) {
    rgl::rgl.clear()
    igraph::rglplot(igraph_obj, layout = layout.kamada.kawai,
                    vertex.color = "navy",
                    vertex.label = colnames(x$adjMat),
                    edge.width = widths,
                    edge.color = color_map)
  } else {
    plot(igraph_obj,
         main = sprintf("Correlation Network | thresh > %s", x$thresh),
         layout = graph.layout,
         vertex.color = node.colors,
         vertex.label = colnames(x$adjMat),
         vertex.shape = "circle",
         vertex.label.color = vertex.label.color,
         vertex.label.dist = vertex.label.dist,
         vertex.size = 10,
         edge.color = color_map,
         edge.lty = 1,
         edge.width = widths,
         edge.curved = 0.01,
         edge.arrow.size = 0.3,
         edge.arrow.mode = "")
  }
}

