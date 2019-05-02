\name{plot.cyEffTest}
\alias{plot.cyEffTest}

\title{ Plot method for a cyEffTest object }
\description{
  \code{plot.cyEffTest}, a method to plot the output of
  \code{\link{cyEffTest}}. It is designed to give a quick overview
  of a \code{cyEffTest} object and to check the assumption of independece between calendar years.
}
\usage{
\method{plot}{cyEffTest}(x, type = "l", xlab = "Z", ylab = "Density", 
                          main = "Calendar Year Effect", col.area = "gray", 
                          border = NA, \dots)
}

\arguments{
  \item{x}{output from \code{cyEffTest}}
  \item{type}{Default: "l". What type of plot should be drawn}
  \item{xlab}{Default: "Z". X axis label}
  \item{ylab}{Default: "Density". Y axis label}
  \item{main}{Default: "Calendar Year Effect". Plot title}
  \item{col.area}{Default: "gray". Color of the shaded area.}
  \item{border}{Default: NULL, the color to draw the border. Use \code{border = NA} to omit borders.}
  \item{\dots}{optional arguments. See \code{\link{plot.default}} for
    more details.} 
}
\details{
 \code{plot.cyEffTest} shows the underlying distribution, the test statistic \eqn{Z} and the relative Confidence Interval.
 If the test statistic \eqn{Z} is within the highlighted region the hypothesis of dependence between calendar years could be rejected.
 
 }
\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See Also \code{\link{cyEffTest}},
\code{\link{summary.cyEffTest}}}
\examples{
plot(cyEffTest(RAA))
}
\keyword{ plot }
\keyword{ methods }

