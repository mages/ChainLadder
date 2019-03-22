\name{plot.dfcor.test}
\alias{plot.dfcor.test}

\title{ Plot method for a dfcor.test object }
\description{
  \code{plot.dfcor.test}, a method to plot the output of
  \code{\link{dfcor.test}}. It is designed to give a quick overview
  of a \code{dfcor.test} object and to check the assumption of independece between development factors.
}
\usage{
\method{plot}{dfcor.test}(x, type = "l", xlab = "T", ylab = "Density", 
                          main = "Development Factor Correlation", col.area = "gray",
                          border = NA, \dots)
}

\arguments{
  \item{x}{output from \code{dfcor.test}}
  \item{type}{Default: "l". What type of plot should be drawn}
  \item{xlab}{Default: "Z". X axis label}
  \item{ylab}{Default: "Density". Y axis label}
  \item{main}{Default: "Development Factor Correlation". Plot title}
  \item{col.area}{Default: "gray". Color of the shaded area.}
  \item{border}{Default: NULL, the color to draw the border. Use \code{border = NA} to omit borders.}
  \item{\dots}{ optional arguments. See \code{\link{plot.default}} for
    more details.} 
}
\details{
 \code{plot.dfcor.test} shows the underlying distribution, the test statistic \eqn{Z} and the relative Confidence Interval.
 If the test statistic \eqn{Z} is within the highlighted region the hypothesis of correlation between development factors could be rejected.
 }
\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See Also \code{\link{dfcor.test}},
\code{\link{summary.dfcor.test}}}
\examples{
plot(dfcor.test(RAA))
}
\keyword{ plot }
\keyword{ methods }

