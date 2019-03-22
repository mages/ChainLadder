\name{check.tr.infl}
\alias{check.tr.infl}
\encoding{UTF-8}
\title{ Check Y-o-Y Triangle Inflation Rates }
\description{
  Check for Year-on-Year Inflation rates down the columns of a run-off triangle
}
\usage{
check.tr.infl(Triangle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{cumulative claims triangle. Assume columns are the development
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
  The function fits an exponential inflation model that takes the form of:
  \deqn{Y=a*(1+b)^x}
  where \eqn{Y} represents the inflated claim amount, \eqn{a} represents the claim amount at the beginning 
  of each period (e.g. AY=0), \eqn{b} is the inflation rate  and \eqn{x} is the time (e.g. AY).
  Fitting such a model on the average level of the case outstanding for each development period, it is 
  possible to appreciate the inflation rate that has affected the average case reserve.
}
\value{
  check.tr.infl returns a list with the following elements
  \item{Triangle}{Input triangle} 
  \item{summ_table}{summary table showing the inflation rate, the \eqn{R^2} of the regression and the number of points used}
  }
\references{
\cite{Berquist, J.R. and Sherman, R.E., Loss Reserve Adequacy Testing: A Comprehensive,
  Systematic Approach, \emph{Proceedings of the Casualty Actuarial Society}, LXIV, 1977, pp.123-184.}
}
\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}

\seealso{ See also \code{\link{qpaid}} for dealing with non-square triangles,  
  \code{\link{inflate.triangle}} to inflate a triangle based on an inflation rate,
  }

\examples{
test <- check.tr.infl( MedMal$MedMalOutstanding / MedMal$MedMalOpen )
plot(test)
summary(test)
print(test)

## For more examples see:
\dontrun{
 demo(check.tr.infl)
}
}
\keyword{tests}