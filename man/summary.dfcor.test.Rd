\name{summary.dfcor.test}
\alias{summary.dfcor.test}

\title{ Summary function for a dfcor.test object }
\description{
  \code{summary} method for a \code{dfcor.test}
  object
}
\usage{
\method{summary}{dfcor.test}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{dfcor.test}}
  \item{\dots}{ optional arguments for a \code{summary} method }
}
\details{
  \code{summary.dfcor.test} shows the summary of a \code{dfcor.test} object.
}
\value{
  \code{summary.dfcor.test} gives a list of two elements back
  \item{Results}{data frame containing the summary statistics}
  \item{Range}{data frame containing the upper and lower limits of the confidence interval range}
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{dfcor.test}},
  \code{\link{plot.dfcor.test}} }
\examples{
 test <- dfcor.test(RAA)
 summary(test)
}
\keyword{ methods }
\keyword{ summary }
