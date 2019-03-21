\name{plot.cyeff.test}
\alias{plot.cyeff.test}

\title{ Plot method for a cyeff.test object }
\description{
  \code{plot.cyeff.test}, a method to plot the output of
  \code{\link{cyeff.test}}. It is designed to give a quick overview
  of a \code{cyeff.test} object and to check the assumption of independece between calendar years.
}
\usage{
\method{plot}{cyeff.test}(x, \dots)
}

\arguments{
  \item{x}{output from \code{cyeff.test}}
  \item{\dots}{ optional arguments. See \code{\link{plot.default}} for
    more details.} 
}
\details{
 \code{plot.cyeff.test} shows the underlying distribution, the test statistic \eqn{Z} and the relative Confidence Interval.
 If the test statistic \eqn{Z} is within the highlighted region the hypothesis of dependence between calendar years could be rejected.
 
 }
\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See Also \code{\link{cyeff.test}},
\code{\link{summary.cyeff.test}}}
\examples{
plot(cyeff.test(RAA))
}
\keyword{ plot }
\keyword{ methods }

