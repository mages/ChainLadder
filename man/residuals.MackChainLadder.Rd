\name{residuals.MackChainLadder}
\alias{residuals.MackChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract residuals of a MackChainLadder model }
\description{
  Extract residuals of a \code{\link{MackChainLadder}} model by 
  origin year, calendar year and development year.
}
\usage{
\method{residuals}{MackChainLadder}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{output of \code{link{MackChainLadder}}}
  \item{\dots}{not in use}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  The function returns a \code{data.frame} of residuals and standardised residuals 
  by origin year, calendar year and development year.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Markus Gesmann }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See Also \code{\link{MackChainLadder}} }

\examples{

RAA <- t(matrix(c(
  5012,  8269, 10907, 11805, 13539, 16181, 18009, 18608, 18662, 18834,
   106,  4285,  5396, 10666, 13782, 15599, 15496, 16169, 16704,    NA,
  3410,  8992, 13873, 16141, 18735, 22214, 22863, 23466,    NA,    NA,
  5655, 11555, 15766, 21266, 23425, 26083, 27067,    NA,    NA,    NA,
  1092,  9565, 15836, 22169, 25955, 26180,    NA,    NA,    NA,    NA,
  1513,  6445, 11702, 12935, 15852,    NA,    NA,    NA,    NA,    NA,
   557,  4020, 10946, 12314,    NA,    NA,    NA,    NA,    NA,    NA,
  1351,  6947, 13112,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
  3133,  5395,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
  2063,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA
  ), ncol=10))
  
 MCL=MackChainLadder(RAA)
 MCL  

 residuals(MCL)

}
\keyword{ models }
