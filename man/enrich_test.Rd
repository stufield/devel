% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrich.test.R
\name{enrich_test}
\alias{enrich_test}
\title{Test for enrichment}
\usage{
enrich_test(x, alternative = c("two.sided", "enrich", "deplete"))
}
\arguments{
\item{x}{A 2x2 confusion matrix (or contingency table) containing
the binary decisions for each contingency. Can also be a (named) list
containing each of the 4 contingencies. See above.}

\item{alternative}{Whether to check for "two.sided" (both Enrich/Deplete)
or specifically one or the other; "enrich" or "deplete".}
}
\value{
Both the comparison to the Hypergeometric Distribution and
The Fisher Exact Test for Count Data with confidence intervals
}
\description{
Calculated whether \verb{2 x 2} table is enriched
for a particular group using Hypergeometric Distribution and
the Fisher's Exact test for count data.
}
\details{
Can also pass a \emph{named} list containing:
\describe{
\item{n11}{The corresponding \link{1,1} position of the table/matrix.}
\item{n1.}{The sum of the top row of the table.}
\item{n.1}{The sum of the first column of the table.}
\item{n}{The sum of the table.}
}
}
\note{
Similar result to Fisher Exact test
}
\examples{
c_mat <- matrix(c(4, 2, 3, 11), ncol = 2)
enrich_test(c_mat)
en_list <- list(n11 = 4, n1. = 7, n.1 = 6, n = 20)
enrich_test(en_list)
}
\seealso{
\code{\link[stats]{dhyper}}, \code{\link[stats]{fisher.test}}
}
\author{
Stu Field
}
