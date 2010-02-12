\name{fit.GeneralMultiChainLadder}
\alias{fit.MultiChainLadder}
\alias{fit.GeneralMultiChainLadder}

\title{ Development Parameters Estimation }
\description{
  These functions fit a system of regression equations in each development period and estimate the parameters needed to complete the triangles and calculate the conditional mean square errors. 
}
\usage{
fit.GeneralMultiChainLadder(triangles,fit.method="SUR",delta=1,...)

fit.MultiChainLadder(triangles,fit.method="SUR",delta=1, extrap="Mack",... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{triangles}{list of input cumulative claims triangles} 
  \item{fit.method}{regression method to estimate the development parameters. Either "SUR" or "OLS". } 
  \item{delta}{weights. Used to determine the variance structure \eqn{D(Y_{i,k}^{-\delta/2})\Sigma_k D(Y_{i,k}^{-\delta/2})}. It defaults to 1. }
  \item{extrap}{how to extrapolate the the covariance matrix in the last period.  If \code{extrap="Mack"}, then \code{mcl} uses the Mack extrapolation method. If \code{"Genvar"}, then it uses the generalized variance method. If the data are  trapezoids, then one should use \code{extrap=FALSE}, where no extrapolation is performed.}
  \item{...}{arguments passed to \code{systemfit}}
}


\value{
  \code{fit.mcl} or \code{fit.GeneralMultiChainLadder}  returns a list of classes \code{"MultiChainLadderFit"} or \code{"GeneralMultiChainLadderFit"} respectively, with the following elements
  \item{triangles}{input triangles of cumulative claims}
  \item{models}{SUR models for each development period}
  \item{B}{estimated development matrix}
  \item{Bcov}{estimated variance-covariance matrix for \eqn{vec(B_k)}}
  \item{ecov}{\eqn{\Sigma_k}}
  \item{fit.method}{estimation method}
  \item{delta}{value of delta}
  \item{extrap}{extrapolation method}

}
\references{
\cite{Zhang Y (2009). A general multivariate chain ladder model.}

\cite{Zhang Y (2010). Prediction error of multivariate reserving models in the chain ladder framework.}
}
\author{Yanwei (Wayne) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See also \link{MultiChainLadder}, \link{GeneralMultiChainLadder}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
