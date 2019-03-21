\name{inflate.triangle}
\alias{inflate.triangle}
\encoding{UTF-8}
\title{ Inflate a Triangle based on an Inflation Rate }
\description{
  Inflate the amounts of a Triangle from the latest diagonal based on an Inflation Rate
}
\usage{
  inflate.triangle(rate, Triangle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rate}{Inflation rate to be applied to the triangle according to an exponential model}
  \item{Triangle}{claim triangle. Assume columns are the development
    period, use transpose otherwise.
    A (mxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i; i=1,\ldots,m; m\geq n }, see
    \code{\link{qpaid}} for how to use (mxn)-development triangles with
    m<n, say higher development period frequency (e.g quarterly) than
    origin period frequency (e.g accident years).}
}
\details{
  The sensitivity of projections of ultimate losses based on incurred loss
  development factors to changes in the adequacy level of case reserves increases
  significantly for the long-tail lines. In particular, if the adequacy of the case reserve
  is changing, the estimates of ultimate losses based on reported claims could be severely distorted.
  The function deflates the amounts of latest diagonal to each diagonal of the triangle according to the inflation rate provided, 
  considering an exponential model.
  The purpose of restating the amounts is to have each diagonal in the triangle at the same level as 
  the latest diagonal (i.e. latest valuation).
  Ideally the metrics that should be restated are average O/S or average claim paid.
}
\value{
  inflate.triangle returns the inflated triangle according to the provided rate 
  }
\references{
\cite{Berquist, J.R. and Sherman, R.E., Loss Reserve Adequacy Testing: A Comprehensive,
  Systematic Approach, \emph{Proceedings of the Casualty Actuarial Society}, LXIV, 1977, pp.123-184.}
}
\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}

\seealso{ See also \code{\link{qpaid}} for dealing with non-square triangles,  
  \code{\link{check.tr.infl}} to check Y-o-Y Triangle Inflation Rates,
  }

\examples{
avg <- MedMal$MedMalOutstanding / MedMal$MedMalOpen
inflated_tr <- inflate.triangle(rate = .15, avg) * MedMal$MedMalOpen + MedMal$MedMalPaid
std_ibnr <- summary(MackChainLadder(MedMal$MedMalReported))$Totals[4, 1]
infl_ibnr <- summary(MackChainLadder(inflated_tr))$Totals[4, 1]

## For more examples see:
\dontrun{
 demo(inflate.triangle)
}
}
\keyword{models}