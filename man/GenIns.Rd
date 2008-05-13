\name{GenIns}
\alias{GenIns}
\docType{data}
\title{
Run off triangle of accumulated general insurance claims data
}
\description{Run off triangle of claims data.
}
\usage{data(GenIns)}
\format{ A matrix with 10 accident years and 10 development years.
}
\source{TAYLOR, G.C. and ASHE, F.R. (1983) Second Moments of Estimates
of Outstanding Claims.  \emph{Journal of Econometrics} \bold{23}, 37-61. 
}
\references{See table 1 in: Distribution-free Calculation of the
  Standard Error of Chain Ladder Reserve Estimates, Thomas Mack, 1993,
  \emph{ASTIN Bulletin} \bold{23}, 213 - 225
}
\examples{
GenIns
matplot(t(GenIns), type="l")
}
\keyword{datasets}
