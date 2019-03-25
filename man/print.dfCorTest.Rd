\name{print.dfCorTest}
\alias{print.dfCorTest}

\title{ Print function for a dfCorTest object }
\description{
  \code{print} method for a \code{dfCorTest}
  object
}
\usage{
\method{print}{dfCorTest}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{dfCorTest}}
  \item{\dots}{ optional arguments for a \code{print} method }
}
\details{
  \code{print.dfCorTest} show the print of a \code{dfCorTest} object.
}
\value{
  \code{print.dfCorTest} displays the default information resulting from 
  a call of the dfCorTest method
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{dfCorTest}},
  \code{\link{plot.dfCorTest}},
  \code{\link{summary.dfCorTest}} }
\examples{
 test <- dfCorTest(RAA)
 print(test)
}
\keyword{ methods }
\keyword{ print }
