\name{tweedieReserve methods}
\alias{print.tweedieReserve}
\alias{summary.tweedieReserve}
\title{
Reserve Risk Capital Report
}
\description{
Main purpose of this function is to create a report to assess the reserve risk 
capital given an object of the \code{\link{tweedieReserve}} class. 
It displays both the ultimate and one year risk views at given percentiles.
}
\usage{
\method{print}{tweedieReserve}(x, ...)
\method{summary}{tweedieReserve}(object, q = c(0.5, 0.75, 0.9, 0.95, 0.995),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    An object of class \code{\link{tweedieReserve}}.
}
\item{object}{
    An object of class \code{\link{tweedieReserve}}.
}
  \item{q}{
    Array of percentiles to be displayed.
    }
 \item{...}{Not used}
}
%\details{
% [tbd]
%}
\value{

  \item{Ultimate_View}{Array of ultimate view reserve risk at the given percentiles.}
  \item{1yr_View}{Array of one year view reserve risk at the given percentiles.}
  \item{Diagnostic}{Quick diagnostic to show the deterministic reserve vs ultimate
        view and one year view best estimate. If the model is working properly, 
        then these three value shouldn't be much different.}
}
%\references{
% n/a
%}
\author{
 Alessandro Carrato MSc FIA OA \email{alessandro.carrato@gmail.com}
}
%\note{
% [tbd]
%}

%\section{Warning}{
%  [tbd]
%}

\seealso{
 See also \code{\link{tweedieReserve}}.
}
\examples{
\dontrun{
tw <- tweedieReserve(MW2008, rereserving = TRUE)
summary(tw)
# For comparison 
CDR.BootChainLadder(BootChainLadder(MW2008))
}
}
\keyword{ print }
