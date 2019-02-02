
#' Compute connected components
#'
#' Internal support function to \code{\link{getStableFeatures}}.
#'
#' @param mat An adjacency matrix calculated with a `kmeans` kernel.
#' @author Michael R. Mehan
#' @noRd
calcConnectedComponents <- function(mat) {
  stopifnot(inherits(mat, "matrix"),
            length(dim(mat)) == 2)
  if ( any(is.na(mat)) ) {
    stop("NAs detected in adjacency matrix: ", sum(is.na(mat)),
         call. = FALSE)
  }
  vertices    <- 1:nrow(mat)
  cluster_vec <- rep(NA, length(vertices))
  unsearched_vertices <- vertices
  i <- 1
  while ( length(unsearched_vertices) > 0 ) {
    cluster_members     <- searchBreadthFirst(mat, unsearched_vertices[1])
    unsearched_vertices <- setdiff(unsearched_vertices, cluster_members)
    cluster_vec[ cluster_members ] <- i
    i <- i + 1
  }
  cluster_vec
}



#' Perform breadth first search
#'
#' Perform breadth first search on an adjacency matrix
#' generated during the stability selection algorithm.
#'
#' @param mat An adjacency matrix.
#' @param start Integer. Which vertex to start the search.
#' @author Michael R. Mehan
#' @noRd
searchBreadthFirst <- function(mat, start = 1) {
  n       <- nrow(mat)
  visited <- rep(0, n)       # vector of adjacent nodes
  n_seq   <- seq(n)          # vector for indexing columns
  queue   <- start           # put starting node as first in queue
  visited[start] <- 1        # starting node
  while ( length(queue) > 0 ) {
    x     <- queue[1]        # tmp variable:1st in queue
    queue <- queue[-1]       # rm 1st in queue from queue
    neighbors <- n_seq[mat[x, ] > 0]    # who r connected to 1st in queue
    neighbors <- neighbors[visited[neighbors] == 0]   # of those connected to 1st in queue, who haven't yet been visited (currently 0)
    visited[neighbors] <- 1        # now set those new neighbors = 1; now been visited
    queue <- c(queue, neighbors)   # add them to queue until queue is empty
  }
  which(visited == 1)
}
