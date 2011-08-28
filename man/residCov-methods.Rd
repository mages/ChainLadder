\name{residCov}
%\docType{genericFunction}
\alias{residCov}
\alias{residCor}
\alias{residCov-methods}
\alias{residCor-methods}
\alias{residCov,MultiChainLadder-method}
\alias{residCor,MultiChainLadder-method}
\title{ Generic function for residCov and residCor}
\description{
 \code{residCov} and  \code{residCov} are a generic functions to extract residual covariance and residual correlation from a system of fitted regressions respectively. 

}

\usage{
residCov(object,...)
residCor(object,...)

\S4method{residCov}{MultiChainLadder}(object,...)
\S4method{residCor}{MultiChainLadder}(object,...)
}

\arguments{
  \item{object}{An object of class "MultiChainLadder". }
  \item{\dots}{ Currently not used. }
  }

\author{ Wayne Zhang \email{actuary_zhang@hotmail.com} }

\seealso{See also  \code{\linkS4class{MultiChainLadder}}. }
\keyword{methods}

