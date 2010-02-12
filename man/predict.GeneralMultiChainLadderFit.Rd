\name{predict.GeneralMultiChainLadderFit}
\alias{predict.GeneralMultiChainLadderFit}
\alias{predict.MultiChainLadderFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction of a claims triangle}
\description{
  The function is  used by \code{GeneralMultiChainLadder} or \code{MultiChainLadder} to forecast future claims.
}
\usage{
\method{predict}{GeneralMultiChainLadderFit}(object,...)

\method{predict}{MultiChainLadderFit}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{output of class \code{GeneralMultiChainLadderFit} or \code{MultiChainLadderFit} from \code{fit.GeneralMultiChainLadder} or \code{fit.MultiChainLadder}. Or could be a list of elements that have at least \code{triangles}, \code{B}, \code{Bcov}, \code{ecov} and \code{delta}.}
  \item{...}{ not in use}
}

\value{
  \item{fullTriangles}{forecasted claims triangle}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Yanwei (Wayne) Zhang \email{actuaryzhang@uchicago.edu}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See Also \link{GeneralMultiChainLadder}, \link{MultiChainLadder} }

\keyword{ models }