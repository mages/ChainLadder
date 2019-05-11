\name{BS.paid.adj}
\alias{BS.paid.adj}
\encoding{UTF-8}
\title{ Berquist-Sherman Paid Claim Development Adjustment }
\description{
  The B-S Paid Claim Development Adjustment methods adjusts paid claims based on the underlying relation between paid and closed claims.
}
\usage{
BS.paid.adj(Triangle.rep.counts = NULL, Triangle.closed, Triangle.paid, 
            ult.counts = NULL, regression.type = "exponential")
}
\arguments{
  \item{Triangle.rep.counts}{cumulative reported claim counts triangle.  Assume columns are the development
    period, use transpose otherwise.
    A (mxn)-matrix \eqn{C_{ik}} which is filled for \eqn{k \leq n+1-i; i=1,\ldots,m; m\geq n }, see
    \code{\link{qpaid}} .}
  \item{Triangle.closed}{cumulative closed claim counts triangle.  Assume columns are the development
    period, use transpose otherwise.}
  \item{Triangle.paid}{cumulative paid claims triangle.  Assume columns are the development
    period, use transpose otherwise.}
  \item{ult.counts}{vector of ultimate claim counts.}  
  \item{regression.type}{Default = "exponential". Type of regression used in the model, it can take 'exponential' or 'linear'. See also 'Details'}
}
\details{
  The importance of recognizing the impact of shifts in the rate of settlement of claims upon historical paid loss data 
  can materially affect the ultimate projections. 

  This functions adjusts the paid claims based on the numerical method described in the B-S paper. 
  
  Berquist and Sherman presented a technique to adjust the paid claim development method for changes in settlement rates.
  The first step of the paid claims adjustment is to determine the disposal rates by accident year and maturity. 
  
  The disposal rate is defined as as the cumulative closed claim counts for each accident year-maturity age cell 
  divided by the selected ultimate claim count for the particular accident year. 
  
  If ultimate claim counts have been provided, they will be used to calulate the disposal rates, otherwise ultimate claim counts will be estimated
  from the cumulative reported  claim counts triangle with a standard development method. 
  
  The disposal rates along the latest diagonal will be selected as the basis for adjusting the closed claim count triangle,
  The selected disposal rate for each maturity are multiplied by the ultimate number of claims to determine the adjusted triangle 
  of closed claim counts. 
  
  Berquist and Sherman then use regression analysis to identify a mathematical formula that approximates the relationship between the cumulative 
  number of closed claims (X) and cumulative paid claims (Y). The algorithm gives the possibility, through the choice of the 'regression.type' field, 
  to fit an exponential model, \eqn{Y = a*e^(bX)}, or a linear model, \eqn{Y = a+b*X}. 
  
  The relation is estimated based on unadjusted closed claim counts and unadjusted paid claims. Once the regression coefficients are estimated, they will be used to
  adjust paid claims based on such coefficients and the adjusted closed claim counts triangle.
  }
\value{
  BS.paid.adj returns the adjusted paid claim triangle
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
# Adjust the Triangle of Paid Claims based on Reported Claim Counts

adj_paid <- BS.paid.adj( Triangle.rep.counts = AutoBI$AutoBIReportedCounts,
                         Triangle.closed = AutoBI$AutoBIClosed,
                         Triangle.paid = AutoBI$AutoBIPaid,
                         regression.type = 'exponential' )

# Calculate the IBNR from the standard unadjusted Paid Triangle

std_ibnr <- summary(MackChainLadder(AutoBI$AutoBIPaid))$Totals[4, 1]

# Calculate the IBNR from the adjusted Paid Triangle

adj_ibnr <- summary(MackChainLadder(adj_paid))$Totals[4, 1]

# Compare the two

adj_ibnr
std_ibnr

## For more examples see:
\dontrun{
 demo(BS.paid.adj)
}
}
\keyword{models}