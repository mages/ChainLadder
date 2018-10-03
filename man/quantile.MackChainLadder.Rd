\name{quantile.MackChainLadder}
\alias{quantile.MackChainLadder}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ quantile function for Mack-chain-ladder }
\description{
  \code{quantile} methods for a \code{MackChainLadder} object
}
\usage{

\method{quantile}{MackChainLadder}(x, probs=c(0.75, 0.95), na.rm = FALSE,
              names = TRUE, type = 7,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object of class \code{"MackChainLadder"}}
  \item{probs}{numeric vector of probabilities with values in [0,1],
    see \code{\link{quantile}} for more help}
  \item{na.rm}{not used}
  \item{names}{not used}
  \item{type}{not used}         
  \item{\dots}{not used}
}
\details{
Reserves at the desired quantile using the Cornish-Fisher expansion. 

The Cornish-Fisher expansion relies on the first three moments of the reserve risk distribution: The Best estimate resulting from the Chain-Ladder projection, the Mack standard deviation and the skewness of the distribution (for skewness estimation, see references below).

The quantile estimation requires only that the standard Mack assumptions are met.

For details of the underlying calculations, see references below.
}
\value{
\code{quantile.MackChainLadder} gives a list with two elements back:
  \item{ByOrigin}{data frame with skewness and quantile statistics by
    origin period}
  \item{Totals}{data frame with total skewness and quantile statistics
    across all origin periods}
}

\references{
\cite{Eric Dal Moro and Yuriy Krvavych.
  Probability of sufficiency of Solvency II Reserve risk margins: Practical approximations. \emph{ASTIN Bulletin}, 47(3), 737-785
  }
  
 \cite{Dal Moro, Eric, A Closed-Form Formula for the Skewness Estimation of Non-Life Reserve Risk Distribution (September 15, 2013). Available at SSRN: https://ssrn.com/abstract=2344297 or http://dx.doi.org/10.2139/ssrn.2344297
 }
}
\author{Eric Dal Moro \email{eric_dal_moro@yahoo.com}}
\seealso{ See also \code{\link{MackChainLadder}} 
}
\examples{
M <- MackChainLadder(GenIns, est.sigma="Mack")
quantile(M, c(0.65, 0.75, 0.9))
}
\keyword{ methods }