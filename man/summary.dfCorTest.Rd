\name{summary.dfCorTest}
\alias{summary.dfCorTest}

\title{ Summary function for a dfCorTest object }
\description{
  \code{summary} method for a \code{dfCorTest}
  object
}
\usage{
\method{summary}{dfCorTest}(object, \dots)
}
\arguments{
  \item{object}{object of class \code{dfCorTest}}
  \item{\dots}{ optional arguments for a \code{summary} method }
}
\details{
  \code{summary.dfCorTest} shows the summary of a \code{dfCorTest} object.
}
\value{
  \code{summary.dfCorTest} gives a list of two elements back
  \item{Results}{data frame containing the summary statistics}
  \item{Range}{data frame containing the upper and lower limits of the confidence interval range}
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{dfCorTest}},
  \code{\link{plot.dfCorTest}} }
\examples{
 test <- dfCorTest(RAA)
 summary(test)
}
\keyword{ methods }
\keyword{ summary }
