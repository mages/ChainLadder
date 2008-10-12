\name{plot.MackChainLadder}
\alias{plot.MackChainLadder}

\title{ Plot method for a MackChainLadder object }
\description{
  \code{plot.MackChainLadder}, a method to plot the output of
  \code{\link{MackChainLadder}}. It is designed to give a quick look at a
  \code{MackChainLadder} object, to check the result and Mack's assumptions.
}
\usage{
\method{plot}{MackChainLadder}(x, mfrow=c(3,2), title=NULL, \dots)
}

\arguments{
  \item{x}{output from \code{MackChainLadder}}
  \item{mfrow}{see \code{\link{par}}}
  \item{title}{see \code{\link{title}}}  
  \item{\dots}{ optional arguments to \code{plot} method} 
}
\details{
 \code{plot.MackChainLadder} shows six graphs, starting from the top left
 with a stacked barchart of the latest claims postion plus IBNR and
 Mack's standard error by origin period; next to it on the right is a
 plot of the forcasted development patterns of all origin periods
 (numbered, starting with 1 for the oldest origin period),
 and 4 residual plots. The residual plots
 show the standard resdiuals against fitted values, origin period,
 calendar period and development period. All residual plot should show
 no pattern or direction for Mack's method to be applicable.
 Pattern in any direction can be the result of trends.
 See
 \cite{Zehnwirth and Barnett. Best estimates for reserves. Proceedings
   of the CAS, LXXXVI I(167), November 2000.} for more details on trends.
}
\author{Markus Gesmann}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See Also \code{\link{MackChainLadder}},
  \code{\link{residuals.MackChainLadder}}}
\examples{
plot(MackChainLadder(RAA))
}
\keyword{ aplot }

