\name{UKMotor}
\alias{UKMotor}
\docType{data}
\title{
UK motor claims triangle
}
\description{
Triangle of cumulative claims payments for four origin (accident) 
years over time (development years).
}
\usage{data("UKMotor")}
\format{
  A matrix with 7 accident years and 7 development years.
}

\source{
\url{https://www.actuaries.org.uk/research-and-resources/documents/claims-reserving-manual-vol2-section-d5-regression-models-based-lo-0}
}
\references{
Stavros Christofides. Regression models based on log-incremental payments. 
Claims Reserving Manual. Volume 2 D5. September 1997
}

\examples{
data(UKMotor)
plot(UKMotor)
MackChainLadder(UKMotor, est.sigma="Mack")
}
\keyword{datasets}
