\name{transf.Bcov}
\alias{transf.Bcov}

\title{ Variance-covariance matrix transformation }
\description{
  The functions transforms the variance-covariance matrix from \code{MultiChainLadder} to the format of \code{GeneralMultiChainLadder}.
}
\usage{
transf.Bcov(Bcov.orig)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Bcov.orig}{the variance-covariance matrix \code{Bcov} from \code{MultiChainLadder} or \code{fit.MultiChainLadder}} 
}
\details{
For N triangles, each \code{Bcov} from \code{MultiChainLadder} is \eqn{N \times N} compared to the \code{Bcov} from \code{gmcl} which is \eqn{N^2 \times N^2}. This functions transforms the \eqn{N \times N} matrix to \eqn{N^2 \times N^2} by filling zeros. The \eqn{(i,j)} element of the original matrix lies on the \eqn{[i+(i-1)N,j+(j-1)N]} element of the transformed matrix. 
}

\author{Yanwei (Wayne) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See also \link{GeneralMultiChainLadder}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
