\name{plot.checkTriangleInflation}
\alias{plot.checkTriangleInflation}

\title{ Plot method for a checkTriangleInflation object }
\description{
  \code{plot.checkTriangleInflation}, a method to plot the output of
  \code{\link{checkTriangleInflation}}. It is designed to give a quick overview
  of a \code{checkTriangleInflation} object and to check the inflation regression curves.
}
\usage{
\method{plot}{checkTriangleInflation}(x, col.line = "black", type = "b", xlab = "dev. period", ylab = NULL, \dots)
}

\arguments{
  \item{x}{output from \code{checkTriangleInflation}}
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
\seealso{ See Also \code{\link{checkTriangleInflation}},
\code{\link{summary.checkTriangleInflation}}}
\examples{
test <- checkTriangleInflation(MedMal$MedMalOutstanding / MedMal$MedMalOpen)
plot(test)
}
\keyword{ plot }
\keyword{ methods }