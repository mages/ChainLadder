\name{print.checkTriangleInflation}
\alias{print.checkTriangleInflation}

\title{ Print function for a checkTriangleInflation object }
\description{
  \code{print} method for a \code{checkTriangleInflation}
  object
}
\usage{
\method{print}{checkTriangleInflation}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{checkTriangleInflation}}
  \item{\dots}{ optional arguments for a \code{print} method }
}
\details{
  \code{print.checkTriangleInflation} show the print of a \code{checkTriangleInflation} object.
}
\value{
  \code{print.checkTriangleInflation} displays the default information resulting from 
  a call of the checkTriangleInflation method
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{checkTriangleInflation}},
  \code{\link{plot.checkTriangleInflation}},
  \code{\link{summary.checkTriangleInflation}} }
\examples{
 test <- checkTriangleInflation(MedMal$MedMalOutstanding / MedMal$MedMalOpen)
 print(test)
}
\keyword{ methods }
\keyword{ print }
