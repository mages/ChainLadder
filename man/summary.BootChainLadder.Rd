\name{summary.BootChainLadder}
\alias{summary.BootChainLadder}
\alias{print.BootChainLadder}
\alias{quantile.BootChainLadder}
\alias{mean.BootChainLadder}
\alias{residuals.BootChainLadder}
\title{ Methods for BootChainLadder objects }

\description{
  \code{summary}, \code{print}, \code{mean}, and \code{quantile}
  methods for \code{BootChainLadder} objects
}
\usage{
\method{summary}{BootChainLadder}(object, probs=c(0.75,0.95), \dots)

\method{print}{BootChainLadder}(x, probs=c(0.75,0.95), \dots)

\method{quantile}{BootChainLadder}(x, probs=c(0.75, 0.95), na.rm = FALSE,
              names = TRUE, type = 7,\dots)

\method{mean}{BootChainLadder}(x, \dots)

\method{residuals}{BootChainLadder}(object, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{output from \code{\link{BootChainLadder}}}
  \item{probs}{numeric vector of probabilities with values in [0,1],
    see \code{\link{quantile}} for more help}
  \item{na.rm}{logical; if true, any \code{NA} and \code{NaN}'s are
    removed from 'x' before the quantiles are computed,
    see \code{\link{quantile}} for more help}
  \item{names}{logical; if true, the result has a \code{names}
    attribute. Set to \code{FALSE} for speedup with many 'probs',
    see \code{\link{quantile}} for more help}
  \item{type}{an integer between 1 and 9 selecting one of the nine quantile
    algorithms detailed below to be used, see \code{\link{quantile}}}         
  \item{\dots}{further arguments passed to or from other methods}
}
\details{
  \code{print.BootChainLadder} calls \code{summary.BootChainLadder} and
  prints a formatted version of the summary.
  \code{residuals.BootChainLadder} gives the residual triangle of
  the expected chain-ladder minus the actual triangle back.

}
\value{
  \code{summary.BootChainLadder}, \code{mean.BootChainLadder}, and
  \code{quantile.BootChainLadder}, give a list with two elements back:

  \item{ByOrigin}{data frame with summary/mean/quantile statistics by
    origin period}
  \item{Totals}{data frame with total summary/mean/quantile statistics
    for all origin period}
}

\author{ Markus Gesmann }
\seealso{ See Also \code{\link{BootChainLadder}} }
\examples{
B <- BootChainLadder(RAA, R=999, process.distr="gamma")
B
summary(B)
mean(B)
quantile(B, c(0.75,0.95,0.99, 0.995))

}
\keyword{ methods }
\keyword{ print }% __ONLY ONE__ keyword per line
