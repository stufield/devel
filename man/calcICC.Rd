% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcICC.R
\name{calcICC}
\alias{calcICC}
\title{Calculate Intraclass Correlation Coefficient}
\usage{
calcICC(x, do.log = FALSE, alpha = 0.05)
}
\arguments{
\item{x}{A named list containing vectors of data representing the
various groups to be tested. One list element for each group.}

\item{do.log}{Logical. Should values be log-transformed.}

\item{alpha}{The significance level for the test.}
}
\value{
A data frame of the ANOVA table. A list of the values for the ICC.
}
\description{
Calculate Intraclass Correlation Coefficient (r_i).
}
\examples{
\dontrun{}
}
\references{
Sokal and Rohlf, pg. 214, 2nd ed.
}
\seealso{
\code{\link{sapply}}, \code{\link{mean}}, \code{\link{sum}}, \code{\link{aov}}, \code{\link{qf}}
}
\author{
Stu Field
}
