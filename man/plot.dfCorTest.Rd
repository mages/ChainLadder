\name{plot.dfCorTest}
\alias{plot.dfCorTest}

\title{ Plot method for a dfCorTest object }
\description{
  \code{plot.dfCorTest}, a method to plot the output of
  \code{\link{dfCorTest}}. It is designed to give a quick overview
  of a \code{dfCorTest} object and to check the assumption of independece between development factors.
}
\usage{
\method{plot}{dfCorTest}(x, type = "l", xlab = "T", ylab = "Density", 
                          main = "Development Factor Correlation", col.area = "gray",
                          border = NA, \dots)
}

\arguments{
  \item{x}{output from \code{dfCorTest}}
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
 \code{plot.dfCorTest} shows the underlying distribution, the test statistic \eqn{Z} and the relative Confidence Interval.
 If the test statistic \eqn{Z} is within the highlighted region the hypothesis of correlation between development factors could be rejected.
 }
\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See Also \code{\link{dfCorTest}},
\code{\link{summary.dfCorTest}}}
\examples{
plot(dfCorTest(RAA))
}
\keyword{ plot }
\keyword{ methods }

