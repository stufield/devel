% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time.R
\name{seconds2time}
\alias{seconds2time}
\alias{time2seconds}
\title{Determine Time from Seconds (& vice versa)}
\usage{
seconds2time(x)

time2seconds(x)
}
\arguments{
\item{x}{\code{character(1)} for \code{\link[=seconds2time]{seconds2time()}}, \code{numeric(1)} for
\code{\link[=time2seconds]{time2seconds()}}. Use format \code{hh:mm:ss.ss} when converting
to seconds. Use 2 decimal point precision to convert to time.}
}
\value{
Either number of seconds (\code{numeric(1)}) or the time format
(\code{character(1)}) as \code{hh:mm:ss.ss}.
}
\description{
\code{\link[=time2seconds]{time2seconds()}} and \code{\link[=seconds2time]{seconds2time()}} determine the
time (format="hh:mm:ss.ss") from the value in seconds or vice versa.
Two digit hour precision optional.
}
\examples{
seconds2time(159.72)

time2seconds("3:44:12.04")
time2seconds("15:44:12.04")
}
\seealso{
\code{\link[=strsplit]{strsplit()}}, \code{\link[=grep]{grep()}}
}
\author{
Stu Field
}
