\name{print.dfcor.test}
\alias{print.dfcor.test}

\title{ Print function for a dfcor.test object }
\description{
  \code{print} method for a \code{dfcor.test}
  object
}
\usage{
\method{print}{dfcor.test}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{dfcor.test}}
  \item{\dots}{ optional arguments for a \code{print} method }
}
\details{
  \code{print.dfcor.test} show the print of a \code{dfcor.test} object.
}
\value{
  \code{print.dfcor.test} displays the default information resulting from 
  a call of the dfcor.test method
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{dfcor.test}},
  \code{\link{plot.dfcor.test}},
  \code{\link{summary.dfcor.test}} }
\examples{
 test <- dfcor.test(RAA)
 print(test)
}
\keyword{ methods }
\keyword{ print }
