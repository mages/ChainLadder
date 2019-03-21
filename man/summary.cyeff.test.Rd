\name{summary.cyeff.test}
\alias{summary.cyeff.test}

\title{ Summary function for a cyeff.test object }
\description{
  \code{summary} method for a \code{cyeff.test}
  object
}
\usage{
\method{summary}{cyeff.test}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{cyeff.test}}
  \item{\dots}{ optional arguments for a \code{summary} method }
}
\details{
  \code{summary.cyeff.test} shows the summary of a \code{cyeff.test} object.
}
\value{
  \code{summary.cyeff.test} gives a list of three elements back
  \item{Table}{data frame containing the statistics for each calendar year}
  \item{Totals}{data frame of totals of the main statistics from the dataframe \code{Table}}
  \item{Range}{data frame containing the upper and lower limits of the confidence interval range}
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{cyeff.test}},
  \code{\link{plot.cyeff.test}} }
\examples{
 test <- cyeff.test(RAA)
 summary(test)
}
\keyword{ methods }
\keyword{ summary }
