#' Independent Components Analysis (ICA)
#'
#' The data matrix X is considered to be a linear combination of
#' non-Gaussian (independent) components i.e. `X = SA` where columns of
#' S contain the independent components and A is a linear mixing
#' matrix. In short, ICA attempts to *un-mix* the data by estimating
#' an un-mixing matrix W where `XW = S`.
#' \cr
#' Under this generative model the measured *signals* in X will tend
#' to be "more Gaussian" than the source components (in S) due to the
#' Central Limit Theorem. Thus, in order to extract the independent
#' components/sources we search for an un-mixing matrix W that
#' maximizes the non-gaussianity of the sources.
#' \cr
#' In FastICA, non-gaussianity is measured using approximations to
#' neg-entropy (J) which are more robust than kurtosis-based measures
#' and fast to compute.
#'
#' @param data A data matrix *n* rows representing
#' observations and *p* columns representing variables.
#' Alternatively a `soma_adat` object. If a data matrix,
#' ensure *only* feature data is being passed. In the latter,
#' the meta data will be removed.
#' @param n.comp Integer. Number of components to be extracted.
#' See [fastICA::fastICA()].
#' @param ... Additional arguments passed to [fastICA::fastICA()].
#' @return A list of:
#' \item{rotation }{The rotation, aptamers.}
#' \item{x }{The ICA projection, samples.}
#' \item{sdev }{Standard deviations, used to calculate variances.}
#' @author Mike Mehan
#' @note There is also an ICA implementation from the \pkg{e1071}
#' package called \code{\link[e1071]{ica}}.
#' @seealso [fastICA::fastICA()], [e1071::ica()]
#' @examples
#' ica <- ICA(sample.adat, n.comp = 5)
#' @export
ICA <- function(data, n.comp, ...) {
  if ( SomaReadr::is.soma_adat(data) ) {
    data %<>% stripMeta()
  }
  data.ica <- fastICA::fastICA(data, n.comp = n.comp, ...)
  rot   <- t(data.ica$A)
  proj  <- data.ica$S
  comps <- paste0("IC", 1:n.comp)
  dimnames(rot)  <- list(colnames(data), comps)
  dimnames(proj) <- list(rownames(data), comps)
  list(rotation = rot, x = proj,
       sdev = rep(1, min(nrow(rot), nrow(proj))))
}
