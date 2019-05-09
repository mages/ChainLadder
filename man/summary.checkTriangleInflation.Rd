\name{summary.checkTriangleInflation}
\alias{summary.checkTriangleInflation}

\title{ Summary function for a checkTriangleInflation object }
\description{
  \code{summary} method for a \code{checkTriangleInflation}
  object
}
\usage{
\method{summary}{checkTriangleInflation}(object, \dots)
}
\arguments{
  \item{object}{object of class \code{checkTriangleInflation}}
  \item{\dots}{ optional arguments for a \code{summary} method }
}
\details{
  \code{summary.checkTriangleInflation} shows the summary of a \code{checkTriangleInflation} object.
}
\value{
  \code{summary.checkTriangleInflation} gives a named numeric array of three rows.
  \item{rate}{Inflation rate for the specific development period}
  \item{R2}{\eqn{R^2} coefficient for each regression curve}
  \item{points}{Number of points used}
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{checkTriangleInflation}},
  \code{\link{plot.checkTriangleInflation}} }
\examples{
 test <- checkTriangleInflation( MedMal$MedMalOutstanding / MedMal$MedMalOpen )
 summary(test)
}
\keyword{ methods }
\keyword{ summary }
