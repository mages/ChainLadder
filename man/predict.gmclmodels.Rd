\name{predict.gmclmodels}
\alias{predict.gmclmodels}
\alias{predict.mclmodels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction of a claims triangle}
\description{
  The function is  used by \code{gmcl} or \code{mcl} to forecast future claims.
}
\usage{
\method{predict}{gmclmodels}(object,...)

\method{predict}{mclmodels}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{output of class \code{gmclmodels} or \code{mclmodels} from \code{fit.gmcl} or \code{fit.mcl}. Or could be a list of elements that have at least \code{triangles}, \code{B}, \code{Bcov}, \code{ecov} and \code{delta}.}
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
\seealso{ See Also \link{gmcl}, \link{mcl} }

\keyword{ models }