\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,MultiChainLadder-method}

\title{ Methods for Function summary}
\description{
 Methods for function \code{summary} to calculate summary statistics from a "MultiChainLadder" object. 
}


\usage{

\S4method{summary}{MultiChainLadder}(object, portfolio=NULL,...)
}

\arguments{
  \item{object}{object of class \code{"MultiChainLadder"}}
  \item{portfolio}{character strings specifying which triangles to be summed up as portfolio.  }
  \item{\dots}{ optional arguments to \code{summary} methods }
}
\details{

 \code{summary} calculations the summary statistics for each triangle and the whole portfolio from \code{portfolio}. \code{portfolio} defaults to the sum of all input triangles. It can also be specified as "i+j" format, which means the sum of the i-th and j-th triangle as portfolio. For example, \code{"1+3"} means the sum of the first and third triangle as portfolio.
  
}
\value{
  For N (\eqn{N \neq 1}) input triangles, \code{summary} gives a list of N+1 elements back, each of which has the following fields:
   \code{Latest} (latest actual claims costs),
    \code{Dev.To.Date} (chain ladder development to date),
    \code{Ultimate} (estimate ultimate claims cost), \code{IBNR}
    (estimated IBNR), \code{S.E} (Estimation of the standard
    error of the IBNR),  \code{CV(IBNR)} (Coefficient of Variation, S.E/IBNR),
     \code{S.E.Est} (Estimation error) and \code{S.E.Proc} (Process error).
}

\author{Wayne (Yanwei) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See Also \code{\link{MultiChainLadder}} }
\examples{
data(GenIns)
fit.bbmw=MultiChainLadder(list(GenIns),fit.method="OLS", mse.method="Independence")
summary(fit.bbmw)

}

\keyword{methods}

