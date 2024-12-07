% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pvalueFDR.R
\name{pvalueFDR}
\alias{pvalueFDR}
\title{Calculate Benjamini-Hochberg FDR Values}
\usage{
pvalueFDR(p, alpha = 0.05, plot = FALSE, return.fdr = FALSE)
}
\arguments{
\item{p}{Vector of p-values.}

\item{alpha}{Alpha level of significance.}

\item{plot}{Logical. Plot the p-values?}

\item{return.fdr}{Logical. Should q-values be returned in the same order as the
p-values were entered?}
}
\value{
A data frame of the step-up procedure OR a vector of FDR-adjusted p-values
if \code{return.fdr=TRUE}. The data frame contains:
\item{p.value }{A sorted vector of the original p-values.}
\item{penalty (k/m)}{The threshold value, corresponding to k / m (slope).}
\item{threshold }{The threshold value, corresponding to alpha * k / m.}
\item{p_hat}{Term in the minimum function corresponding to p * m / k.}
\item{fdr }{The FDR-adjusted p-values.}
\item{alpha }{The chosen significance threshold.}
}
\description{
Diagnostic and discovery plots to explain how q-value calculations work via
Benjemani-Hochberg "step-up" correction procedure. FDR corrected p-values are
calculated from scratch and optionally returned.
}
\note{
You're basically solving for the slope (k/m) that makes the p-value (alpha)
significant.
}
\examples{
p1 <- c(0.01, 0.013, 0.014, 0.19, 0.35, 0.5, 0.63, 0.67, 0.75, 0.81)
fdr1 <- pvalueFDR(p1, plot = TRUE)
fdr2 <- p.adjust(p1, method = "fdr")
all.equal(fdr1$fdr, fdr2)

set.seed(1001)
p2 <- runif(10, 0.0005, 0.3)
pvalueFDR(p2, plot = TRUE)

set.seed(666)
p3 <- c(runif(10), rep(0.5, 10))
pvalueFDR(p3, plot = TRUE)

p4 <- c(runif(200), runif(25, 0.01, 0.1))
pvalueFDR(p4, plot = TRUE)
}
\references{
http://www.unc.edu/courses/2007spring/biol/145/001/docs/lectures/Nov12.html \cr
http://en.wikipedia.org/wiki/False_discovery_rate#Benjamini.E2.80.93Hochberg_procedure

Benjamini, Y., and Hochberg, Y. (1995). Controlling the false
discovery rate: a practical and powerful approach to multiple
testing. \emph{Journal of the Royal Statistical Society Series B}
\bold{57}, 289-300.
}
\seealso{
\code{\link[stats]{p.adjust}}, \code{\link{cummin}}
}
\author{
Stu Field
}
\keyword{FDR}
