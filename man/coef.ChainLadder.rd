\name{coef.ChainLadder}
\alias{coef.ChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract residuals of a ChainLadder model }
\description{
  Extract residuals of a \code{\link{MackChainLadder}} model by 
  origin-, calendar- and development period.
}
\usage{
\method{coef}{ChainLadder}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{output of the \code{\link{chainladder}} function}
  \item{\dots}{optional arguments which may become named
    attributes of the resulting vector}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  The function returns a vector of the single-parameter coefficients -- also
  called age-to-age (ATA) or report-to-report (RTR) factors --
  of the models produced by running the 'chainladder' function.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Dan Murphy }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See Also \code{\link{chainladder}} }

\examples{

coef(chainladder(RAA))

}
\keyword{ models }
