\name{summary.MackChainLadder}
\alias{summary.MackChainLadder}
\alias{print.MackChainLadder}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Summary and print function for Mack-chain-ladder }
\description{
  \code{summary} and \code{print} methods for a \code{MackChainLadder}
  object
}
\usage{

\method{summary}{MackChainLadder}(object, \dots)

\method{print}{MackChainLadder}(x, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{object of class \code{"MackChainLadder"}}
  \item{\dots}{ optional arguments to \code{print} or \code{summary} methods }
}
\details{
  \code{print.MackChainLadder} calls \code{summary.MackChainLadder} and
  prints a formatted version of the summary.
}
\value{
  \code{summary.MackChainLadder} gives a list of two elements back
  \item{ByOrigin}{data frame with \code{Latest} (latest actual claims costs),
    \code{Dev.To.Date} (chain ladder development to date),
    \code{Ultimate} (estimate ultimate claims cost), \code{IBNR}
    (estimated IBNR), \code{Mack.S.E} (Mack's estimation of the standard
    error of the IBNR), and \code{CV(IBNR)} (Coefficient of Variance, Mack.S.E/IBNR)}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
\keyword{ print }% __ONLY ONE__ keyword per line
