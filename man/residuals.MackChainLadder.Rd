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

 data(RAA)
 MCL=MackChainLadder(RAA)
 MCL  

 residuals(MCL)

}
\keyword{ models }
