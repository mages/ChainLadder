\name{plot.check.tr.infl}
\alias{plot.check.tr.infl}

\title{ Plot method for a check.tr.infl object }
\description{
  \code{plot.check.tr.infl}, a method to plot the output of
  \code{\link{check.tr.infl}}. It is designed to give a quick overview
  of a \code{check.tr.infl} object and to check the inflation regression curves.
}
\usage{
\method{plot}{check.tr.infl}(x, col.line = "black", type = "b", xlab = "dev. period", ylab = NULL, \dots)
}

\arguments{
  \item{x}{output from \code{check.tr.infl}}
  \item{col.line}{Default: "black". Line color}
  \item{type}{Default: "b". What type of plot should be drawn}
  \item{xlab}{Default: "dev. period". X axis label}
  \item{ylab}{Default: NULL. Y axis label}
  \item{\dots}{optional arguments. See \code{\link{plot.default}} for
    more details.} 
}
\details{
 \code{plot.cyeff.test} plots all the columns of the triangle (Development Periods) against the regression curves.
 }
\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See Also \code{\link{check.tr.infl}},
\code{\link{summary.check.tr.infl}}}
\examples{
test <- check.tr.infl(MedMal$MedMalOutstanding / MedMal$MedMalOpen)
plot(test)
}
\keyword{ plot }
\keyword{ methods }