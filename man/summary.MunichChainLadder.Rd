\name{summary.MunichChainLadder}
\alias{summary.MunichChainLadder}
\alias{print.MunichChainLadder}

\title{ Summary and print function for Munich-chain-ladder}
\description{
  Summarise a \code{MunichChainLadder} object;
  Print a \code{MunichChainLadder} object

}
\usage{
\method{summary}{MunichChainLadder}(object, \dots)

\method{print}{MunichChainLadder}(x, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{object of class \code{"MunichChainLadder"}}
  \item{\dots}{ optional arguments to \code{print} or \code{summary} methods }
}
\details{
    \code{print.MunichChainLadder} calls \code{summary.MunichChainLadder} and
  prints a formatted version of the summary.
}

\value{
  \code{summary.MunichChainLadder} gives a list of two elements back
  \item{ByOrigin}{data frame with \emph{Latest Paid} (latest actual paid
    claims costs), \emph{Latest Incurred} (latest actual incurred
    claims position),\emph{Latest P/I Ratio} (ratio of latest
    paid/incurred claims),  \emph{Ult. Paid} (estimate ultimate claims
    cost based on the paid triangle), \emph{Ult. Incurred} (estimate
    ultimate claims cost based on the incurred triangle),\emph{Ult. P/I
      Ratio} (ratio of ultimate paid forecast / utimate incurred forecast)
  }
  \item{Totals}{data frame of totals over all origin periods. The items
    follow the same naming convention as in \code{ByOrigin} above
  }
}
\author{ Markus Gesmann }
\seealso{ See Also \code{\link{MunichChainLadder}},
  \code{\link{plot.MunichChainLadder}} }
\examples{
M <- MunichChainLadder(MCLpaid, MCLincurred)
M
summary(M)
summary(M)$ByOrigin
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
\keyword{ print }% __ONLY ONE__ keyword per line
