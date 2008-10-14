\name{MCLpaid}
\alias{MCLpaid}
\alias{MCLincurred}
\docType{data}
\title{Run off triangles of accumulated paid and incurred claims data.}
\description{Run-off triangles based on a fire portfolio}
\usage{data(MCLpaid)
data(MCLincurred)}
\format{
  A matrix with 7 origin years and 7 development years.
}
\source{
  \cite{
    Gerhard Quarg and Thomas Mack. Munich Chain Ladder. \emph{Blatter DGVFM}. 26,
    Munich, 2004.
  }
}
\examples{
MCLpaid
MCLincurred
op=par(mfrow=c(2,1))
matplot(t(MCLpaid), type="l")
matplot(t(MCLincurred), type="l")
par(op)
}
\keyword{datasets}
