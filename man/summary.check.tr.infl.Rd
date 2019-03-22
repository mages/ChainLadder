\name{summary.check.tr.infl}
\alias{summary.check.tr.infl}

\title{ Summary function for a check.tr.infl object }
\description{
  \code{summary} method for a \code{check.tr.infl}
  object
}
\usage{
\method{summary}{check.tr.infl}(object, \dots)
}
\arguments{
  \item{object}{object of class \code{check.tr.infl}}
  \item{\dots}{ optional arguments for a \code{summary} method }
}
\details{
  \code{summary.check.tr.infl} shows the summary of a \code{check.tr.infl} object.
}
\value{
  \code{summary.check.tr.infl} gives a named numeric array of three rows.
  \item{rate}{Inflation rate for the specific development period}
  \item{R2}{\eqn{R^2} coefficient for each regression curve}
  \item{points}{Number of points used}
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{check.tr.infl}},
  \code{\link{plot.check.tr.infl}} }
\examples{
 test <- test <- check.tr.infl( MedMal$MedMalOutstanding / MedMal$MedMalOpen )
 summary(test)
}
\keyword{ methods }
\keyword{ summary }
