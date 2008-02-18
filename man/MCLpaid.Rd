\name{MCLpaid}
\alias{MCLpaid}
\alias{MCLincurred}
\docType{data}
\title{Run off triangles of accumulated paid and incurred claims data.}
\description{Run-off triangles of Automatic Factultative business in General
  Liability}
\usage{data(MCLpaid)
data(MCLincurred)}
\format{
  A matrix with 7 origin years and 7 development years.
}
\source{
  Gerhard Quarg and Thomas Mack. Munich Chain Ladder. Blatter DGVFM 26,
  Munich, 2004. 
  }
\examples{
data(MCLpaid)
data(MCLincurred)
op=par(mfrow=c(2,1))
matplot(t(MCLpaid), type="l")
matplot(t(MCLincurred), type="l")
par(op)
}
\keyword{datasets}
