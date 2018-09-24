\name{summary.MackChainLadder}
\alias{summary.MackChainLadder}
\alias{print.MackChainLadder}
\alias{quantile.MackChainLadder}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Summary and print function for Mack-chain-ladder }
\description{
  \code{summary}, \code{print} and \code{quantile} methods for a \code{MackChainLadder}
  object
}
\usage{

\method{summary}{MackChainLadder}(object, \dots)

\method{print}{MackChainLadder}(x, \dots)

\method{quantile}{MackChainLadder}(x, probs=c(0.75, 0.95), na.rm = FALSE,
              names = TRUE, type = 7,\dots)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{object of class \code{"MackChainLadder"}}
   \item{type}{an integer between 1 and 9 selecting one of the nine quantile
    algorithms detailed below to be used, see \code{\link{quantile}}}         
  \item{\dots}{ optional arguments to \code{print}, \code{summary} or \code{quantile} methods }
    \item{probs}{numeric vector of probabilities with values in [0,1],
    see \code{\link{quantile}} for more help}
  \item{na.rm}{logical; if true, any \code{NA} and \code{NaN}'s are
    removed from 'x' before the quantiles are computed,
    see \code{\link{quantile}} for more help}
  \item{names}{logical; if true, the result has a \code{names}
    attribute. Set to \code{FALSE} for speedup with many 'probs',
    see \code{\link{quantile}} for more help}
}
\details{
  \code{print.MackChainLadder} calls \code{summary.MackChainLadder} and
  prints a formatted version of the summary.
}
\value{
  \code{summary.MackChainLadder} gives a list of two elements back
  \item{ByOrigin}{data frame with \code{Latest} (latest actual claims costs),
    \code{Dev.To.Date} (chain-ladder development to date),
    \code{Ultimate} (estimated ultimate claims cost), \code{IBNR}
    (estimated IBNR), \code{Mack.S.E} (Mack's estimation of the standard
    error of the IBNR), and \code{CV(IBNR)} (Coefficient of Variance=Mack.S.E/IBNR)}
  \item{Totals}{data frame of totals over all origin periods. The items
    follow the same naming convention as in \code{ByOrigin} above}
}

\author{ Markus Gesmann }
\seealso{ See also \code{\link{MackChainLadder}},
  \code{\link{plot.MackChainLadder}} }
\examples{
 R <- MackChainLadder(RAA)
 R
 summary(R)
 summary(R)$ByOrigin$Ultimate
 quantile(R, probs=c(0.5, 0.75, 0.9))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
\keyword{ print }% __ONLY ONE__ keyword per line
