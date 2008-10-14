\name{plot.MunichChainLadder}
\alias{plot.MunichChainLadder}

\title{  Plot method for a MunichChainLadder object }
\description{
  \code{plot.MunichChainLadder}, a method to plot the output of
  \code{\link{MunichChainLadder}}. It is designed to give a quick look at a
  \code{MunichChainLadder} object, to check the result and the
  correlation between the paid and incurred residuals.
}
\usage{

\method{plot}{MunichChainLadder}(x,  mfrow=c(2,2), title=NULL, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{output from \code{MunichChainLadder}}
  \item{mfrow}{see \code{\link{par}}}
  \item{title}{see \code{\link{title}}}  
  \item{\dots}{ optional arguments to \code{plot} method }
}
\details{
  \code{plot.MunichChainLadder} shows four plots, starting from the top
  left with a barchart of forecasted ultimate claims costs by
  Munich-chain-ladder (MCL) on paid and incurred data by origin period;
  the barchart next to it compares the ratio of forecasted ultimate
  claims cost on paid and incurred data based on the Mack-chain-ladder and
  Munich-chain-ladder methods; the two residual plots at the bottom show the
  correlation of (incurred/paid)-chain-ladder factors against the
  paid-chain-ladder factors and the correlation of
  (paid/incurred)-chain-ladder factors against the incurred-chain-ladder
  factors.
}
\author{ Markus Gesmann}
\note{ The design of the plots follows those of Quarg's paper:
    \cite{Gerhard Quarg and Thomas Mack. Munich Chain Ladder. Blatter DGVFM 26,
  Munich, 2004.}
}
\seealso{ See Also as \code{\link{MunichChainLadder}}}
\examples{
M <- MunichChainLadder(MCLpaid, MCLincurred)
plot(M)
}
\keyword{ aplot }
