\name{print.cyEffTest}
\alias{print.cyEffTest}

\title{ Print function for a cyEffTest object }
\description{
  \code{print} method for a \code{cyEffTest}
  object
}
\usage{
\method{print}{cyEffTest}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{cyEffTest}}
  \item{\dots}{ optional arguments for a \code{print} method }
}
\details{
  \code{print.cyEffTest} show the print of a \code{cyEffTest} object.
}
\value{
  \code{print.cyEffTest} displays the default information resulting from 
  a call of the cyEffTest method
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{cyEffTest}},
  \code{\link{plot.cyEffTest}},
  \code{\link{summary.cyEffTest}} }
\examples{
 test <- cyEffTest(RAA)
 print(test)
}
\keyword{ methods }
\keyword{ print }
