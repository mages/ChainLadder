\name{print.check.tr.infl}
\alias{print.check.tr.infl}

\title{ Print function for a check.tr.infl object }
\description{
  \code{print} method for a \code{check.tr.infl}
  object
}
\usage{
\method{print}{check.tr.infl}(x, \dots)
}
\arguments{
  \item{x}{object of class \code{check.tr.infl}}
  \item{\dots}{ optional arguments for a \code{print} method }
}
\details{
  \code{print.check.tr.infl} show the print of a \code{check.tr.infl} object.
}
\value{
  \code{print.check.tr.infl} displays the default information resulting from 
  a call of the check.tr.infl method
}

\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\seealso{ See also \code{\link{check.tr.infl}},
  \code{\link{plot.check.tr.infl}},
  \code{\link{summary.check.tr.infl}} }
\examples{
 test <- check.tr.infl(MedMal$MedMalOutstanding / MedMal$MedMalOpen)
 print(test)
}
\keyword{ methods }
\keyword{ print }
