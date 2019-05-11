\name{cyEffTest}
\alias{cyEffTest}
\encoding{UTF-8}
\title{ Testing for Calendar Year Effect }
\description{
  One of the three basic assumptions underlying the chain ladder
  method is the independence of the accident years. 
  The function tests this assumption.
}
\usage{
cyEffTest(Triangle, ci = 0.95)
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
  \item{ci}{confidence interval. Default: .95. A confidence interval is 
  a type of interval estimate, computed from the statis tics of the observed data, 
  that might contain the true value of an unknown population parameter. 
  The interval has an associated confidence level that quantifies 
  the level of confidence that the parameter lies in the interval. 
  }
}
\details{
  The main reason why this independence can be violated in practice is the fact that 
  there could be certain calendar year effects such as major changes in claims handling or in 
  case reserving or external influences such as substantial changes in court decisions or 
  inflation.\cr
  As described by the Mack's 1994 paper a procedure is designed to test for calendar year 
  influences.\cr
  The procedure returns a summary statistic \eqn{Z} which is assumed to be Normally Distributed.
  It is therefore possible to define a confidence interval threshold in order to evaluate the 
  outcome of the test.
}
\value{
  cyEffTest returns a list with the following elements
  \item{test_table}{complete table of results} 
  \item{Z}{summary statistic}
  \item{E}{expected value of the resulting distribution}
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

}
\seealso{ See also \code{\link{qpaid}} for dealing with non-square triangles,  
  \code{\link{dfCorTest}} for the test for correlations between subsequent development factors,
  \code{\link{chainladder}} for the chain-ladder method,
  \code{\link{summary.cyEffTest}},  
  \code{\link{plot.cyEffTest}}
  }

\examples{
# Before actually applying the Chain Ladder technique it is necessary to check
# wether the triangle has Calendar Year Effect

# Apply the function to the triangle and save the output into the variable test
test <- cyEffTest(RAA)

# Plot the confidence interval and the test metric
plot(test)

# The metric is within the confidence interval, therefore the triangle doesn't
# have Calendar Year Effect

# Print the summary table
summary(test)

# Print only the main outcomes
print(test)
# The test has returned a negative outcome. This means that the triangle is 
# not affected by Caledar Year Effect and therefore the chain ladder method 
# can be applied.
}
\keyword{tests}