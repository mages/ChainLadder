\name{GenIns}
\alias{GenIns}
\alias{GenInsLong}
\docType{data}
\title{
  Run off triangle of claims data.
}
\description{
  Run off triangle (\code{GenIns}) of accumulated general insurance claims
  data. \code{GenInsLong} provides the same data in a 'long' format.
}
\usage{GenIns}
\format{ A matrix with 10 accident years and 10 development years.
}
\source{
	\cite{TAYLOR, G.C. and ASHE, F.R. (1983) Second Moments of Estimates
of Outstanding Claims.  \emph{Journal of Econometrics} \bold{23}, 37-61. }
}
\references{See table 1 in: 
\cite{Distribution-free Calculation of the
  Standard Error of Chain Ladder Reserve Estimates, Thomas Mack, 1993,
  \emph{ASTIN Bulletin} \bold{23}, 213 - 225}
}
\examples{
GenIns
plot(GenIns)

## Convert long format into triangle
## Triangles are usually stored as 'long' tables in data bases
as.triangle(GenInsLong, origin="accyear", dev="devyear", "incurred claims")

}
\keyword{datasets}
