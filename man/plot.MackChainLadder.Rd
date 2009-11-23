\name{plot.MackChainLadder}
\alias{plot.MackChainLadder}

\title{ Plot method for a MackChainLadder object }
\description{
  \code{plot.MackChainLadder}, a method to plot the output of
  \code{\link{MackChainLadder}}. It is designed to give a quick overview
  of a \code{MackChainLadder} object and to check Mack's model assumptions.
}
\usage{
\method{plot}{MackChainLadder}(x, mfrow=c(3,2), title=NULL, lattice=FALSE,\dots)
}

\arguments{
  \item{x}{output from \code{MackChainLadder}}
  \item{mfrow}{see \code{\link{par}}}
  \item{title}{see \code{\link{title}}}  
  \item{lattice}{logical. Default is set to \code{FALSE} and plots as
  described in the details section are produced. If \code{lattice=TRUE}, the function
  \code{\link{xyplot}} of the lattice package is used to plot
  developments by origin period in different panels, plus Mack's S.E.}  
  \item{\dots}{ optional arguments. See \code{\link{plot.default}} for
    more details.} 
}
\details{
 \code{plot.MackChainLadder} shows six graphs, starting from the top left
 with a stacked bar-chart of the latest claims position plus IBNR and
 Mack's standard error by origin period; next right to it is a
 plot of the forecasted development patterns for all origin periods
 (numbered, starting with 1 for the oldest origin period),
 and 4 residual plots. The residual plots
 show the standardised residuals against fitted values, origin period,
 calendar period and development period. All residual plot should show
 no patterns or directions for Mack's method to be applicable.
 Pattern in any direction can be the result of trends and should be
  further investigated, see
 \cite{Barnett and Zehnwirth. Best estimates for reserves. Proceedings
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

