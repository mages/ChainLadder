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
  The format is:
 triangle [1:7, 1:7] 3511 4001 4355 4295 4150 ...
 - attr(*, "dimnames")=List of 2
  ..$ origin: chr [1:7] "2007" "2008" "2009" "2010" ...
  ..$ dev   : chr [1:7] "1" "2" "3" "4" ...
 - attr(*, "class")= chr [1:2] "triangle" "matrix"
}

\source{
\url{http://www.actuaries.org.uk/research-and-resources/documents/claims-reserving-manual-vol2-section-d5-regression-models-based-lo-0}
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
