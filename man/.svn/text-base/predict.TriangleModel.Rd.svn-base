\name{predict.TriangleModel}
\alias{predict.TriangleModel}
\alias{predict.ChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction of a claims triangle}
\description{
  The function is internally used by \code{\link{MackChainLadder}} to forecast future claims.
}
\usage{
\method{predict}{TriangleModel}(object,...)
\method{predict}{ChainLadder}(object,...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a list with two items: \code{Models}, \code{Triangle}
  \describe{
    \item{\code{Models}}{list of linear models for each development period
      }
    \item{\code{Triangle}}{input triangle to forecast
      }
    }
  }
  \item{\dots}{ not in use}
}
%\details{ \code{Models}  list linear models
%
%  
%}
\value{
  \item{FullTriangle}{forecasted claims triangle}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Markus Gesmann}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See also \code{\link{chainladder}}, \code{\link{MackChainLadder}} }
\examples{

RAA

CL <- chainladder(RAA)
CL
predict(CL)
}
\keyword{ models }