\name{summary.cyEffTest}
\alias{summary.cyEffTest}

\title{ Summary function for a cyEffTest object }
\description{
  \code{summary} method for a \code{cyEffTest}
  object
}
\usage{
\method{summary}{cyEffTest}(object, \dots)
}
\arguments{
  \item{object}{object of class \code{cyEffTest}}
  \item{\dots}{ optional arguments for a \code{summary} method }
}
\details{
  \code{summary.cyEffTest} shows the summary of a \code{cyEffTest} object.
}
\value{
  \code{summary.cyEffTest} gives a list of three elements back
  \item{Table}{data frame containing the statistics for each calendar year}
  \item{Totals}{data frame of totals of the main statistics from the dataframe \code{Table}}
  \item{Range}{data frame containing the upper and lower limits of the confidence interval range}
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{cyEffTest}},
  \code{\link{plot.cyEffTest}} }
\examples{
 test <- cyEffTest(RAA)
 summary(test)
}
\keyword{ methods }
\keyword{ summary }
