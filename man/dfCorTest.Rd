\name{dfCorTest}
\alias{dfCorTest}
\encoding{UTF-8}
\title{ Testing for Correlations between Subsequent Development Factors }
\description{
  One of the main assumptions underlying the chain ladder
  method is the uncorrelation of subsequest development factor. 
  The function tests this assumption.
}
\usage{
dfCorTest(Triangle, ci = .5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{cumulative claims triangle.  Assume columns are the development
    period, use transpose otherwise.
    A (mxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i; i=1,\ldots,m; m\geq n }, see
    \code{\link{qpaid}} for how to use (mxn)-development triangles with
    m<n, say higher development period frequency (e.g quarterly) than
    origin period frequency (e.g accident years).}
  \item{ci}{confidence interval. Default: .5. A confidence interval is 
  a type of interval estimate, computed from the statis tics of the observed data, 
  that might contain the true value of an unknown population parameter. 
  The interval has an associated confidence level that quantifies 
  the level of confidence that the parameter lies in the interval. 
  }
}
\details{
  As described by the Mack's 1994 paper a procedure is designed to test for calendar year 
  influences.
  The usual test for uncorrelatedness requires that we have identically distributed pairs of 
  observations which come from a  Normal distribution. Both conditions are usually not fulfilled
  for adjacent columns of development factors. Spearman's correlation coefficient is therefore used.
  The metric calulated by the procudeure described return a statistic \eqn{T} that it is assumed to
  be Normally Distributed. It is therefore possible to define a confidence interval threshold in order 
  to evaluate the outcome of the test.
}
\value{
  dfCorTest returns a list with the following elements
  \item{T_stat}{summary statistic}
  \item{Var}{variance of the resulting distribution}
  \item{Range}{vector of the range corresponding the confidence interval threshold selected}
  \item{ci}{confidence interval}
  }
\references{
\cite{Mack, T., Measuring the Variability of Chain Ladder Reserve Estimates, \emph{Casualty Actuarial Society Forum}, Spring 1994}

}
\author{Marco De Virgilis \email{devirgilis.marco@gmail.com}}
\note{
  Additional references for further reading:
  
  \cite{Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. \emph{Astin Bulletin}. Vol. 23. No 2. 1993. pp.213:225}

  \cite{Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. \emph{Astin Bulletin}. Vol. 29. No 2. 1999. pp.361:366}
  
  \cite{Venter, G.G., Testing the Assumptions of Age-to-Age Factors, \emph{Proceedings of the Casualty Actuarial Society} LXXXV, 1998, pp. 807-847}

}
\seealso{ See also \code{\link{qpaid}} for dealing with non-square triangles,  
  \code{\link{cyEffTest}} for the test for calendar year effect,
  \code{\link{chainladder}} for the chain-ladder method,
  \code{\link{summary.dfCorTest}},  
  \code{\link{plot.dfCorTest}}
  }

\examples{
# Test the Triangle for Correlation between subsequent development factor

test<-dfCorTest(RAA)

# Plot the results
plot(test)

# Summary of the calculations
summary(test)
print(test)

## For more examples see:
\dontrun{
 demo(dfCorTest)
}
}
\keyword{tests}