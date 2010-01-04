\name{summary.gmcl}
\alias{summary.gmcl}
\alias{print.gmcl}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Summary and print function for class gmcl }
\description{
  \code{summary} and \code{print} methods for a \code{gmcl}
  object
}
\usage{

\method{summary}{gmcl}(object, agg=NULL, ...)

\method{print}{gmcl}(x, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, object}{object of class \code{"gmcl"}}
  \item{agg}{character strings specifying which triangles to be summed up as portfolio. Default to all triangles.}
  \item{\dots}{ optional arguments to \code{print} or \code{summary} methods }
}
\details{

 \code{summary.gmcl} calculations the summary statistics for each triangle and the whole portfolio from \code{agg}. \code{agg} defaults to the sum of all input triangles. It can also be specified as "i+j" format, which means the sum of the i-th and j-th triangle as portfolio. 
  
  \code{print.gmcl} calls \code{summary.gmcl} and
  prints a formatted version of the summary. 

  
}
\value{
  For N input triangles, \code{summary.gmcl} gives a list of N+1 elements back, each of which has the following fields:
   \code{Latest} (latest actual claims costs),
    \code{Dev.To.Date} (chain ladder development to date),
    \code{Ultimate} (estimate ultimate claims cost), \code{IBNR}
    (estimated IBNR), \code{S.E} (Estimation of the standard
    error of the IBNR),  \code{CV(IBNR)} (Coefficient of Variance, S.E/IBNR),
     \code{S.E.Est} (Estimation error) and \code{S.E.Proc} (Process error).
}

\author{Yanwei (Wayne) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See Also \link{gmcl}, \link{mcl} }
\examples{
data(GenIns)
fit.bbmw=mcl(list(GenIns),fit.method="OLS", delta=1, 
		extrap="Mack", mse.method="Murphy")

fit.bbmw$B
summary.gmcl(fit.bbmw)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
\keyword{ print }% __ONLY ONE__ keyword per line
