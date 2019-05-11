\name{checkTriangleInflation}
\alias{checkTriangleInflation}
\encoding{UTF-8}
\title{ Check Y-o-Y Triangle Inflation Rates }
\description{
  Check for Year-on-Year Inflation rates down the columns of a run-off triangle
}
\usage{
checkTriangleInflation(Triangle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{average claim amounts triangle. Assume columns are the development
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
  is changing, the estimates of ultimate losses based on reported claims could be severely distorted.\cr
  The function fits an exponential inflation model that takes the form of:
  \deqn{Y=a*(1+b)^x}
  where \eqn{Y} represents the inflated claim amount, \eqn{a} represents the claim amount at the beginning 
  of each period (e.g. AY=0), \eqn{b} is the inflation rate  and \eqn{x} is the time (e.g. AY).\cr
  Fitting such a model on the average level of the case outstanding (or any other average claim amount) 
  for each development period, it is possible to appreciate the inflation rate that has affected the average case reserve.\cr
  It is necessary to check the inflation on average amounts, otherwise the estimates may be distorted due to an increase in    the number of claims rather than an actual increase in the inflation level. \cr
  If the level of inflation is material, it would be necessary to adjust each cell in the triangle.\cr 
  This is to to have each diagonal in the triangle at the same level as the latest diagonal (i.e. latest valuation). This
  adjustment would prevent distortions in the estimates caused by inflation and not by actual variations in the claim
  experience.
}
\value{
  checkTriangleInflation returns a list with the following elements
  \item{Triangle}{Input triangle} 
  \item{summ_table}{summary table showing the inflation rate, the \eqn{R^2} of the regression and the number of points used}
  }
\references{
\cite{Berquist, J.R. and Sherman, R.E., Loss Reserve Adequacy Testing: A Comprehensive,
  Systematic Approach, \emph{Proceedings of the Casualty Actuarial Society}, LXIV, 1977, pp.123-184.}
}
\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}

\seealso{ See also \code{\link{qpaid}} for dealing with non-square triangles,  
  \code{\link{inflateTriangle}} to inflate a triangle based on an inflation rate,
  }

\examples{
# Create a triangle of average outstanding claims as the ratio between O/S Claims 
# and Open Claims (i.e. the number of outstanding claims)
avg <- MedMal$MedMalOutstanding / MedMal$MedMalOpen

# Check the level of average inflation Y-o-Y
test<-checkTriangleInflation(avg)

# Plot the results
# A model of exponential inflation fits quite well the level of average O/S claims
# This is particularly evident for DP 1,2,3
plot(test)

# Get the summary in an analytical way to observe the ratios and the number of points used
summary(test)

# Print the output
print(test) 
# There is an inflation level equal to .15 at the first development period. It would be 
# appropriate to adjust the triangle before proceeding with any estimate method.
}
\keyword{tests}