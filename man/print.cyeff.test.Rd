\name{print.cyeff.test}
\alias{print.cyeff.test}

\title{ Print function for a cyeff.test object }
\description{
  \code{print} method for a \code{cyeff.test}
  object
}
\usage{
\method{print}{cyeff.test}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{cyeff.test}}
  \item{\dots}{ optional arguments for a \code{print} method }
}
\details{
  \code{print.cyeff.test} show the print of a \code{cyeff.test} object.
}
\value{
  \code{print.cyeff.test} displays the default information resulting from 
  a call of the cyeff.test method
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{cyeff.test}},
  \code{\link{plot.cyeff.test}},
  \code{\link{summary.cyeff.test}} }
\examples{
 test <- cyeff.test(RAA)
 print(test)
}
\keyword{ methods }
\keyword{ print }
