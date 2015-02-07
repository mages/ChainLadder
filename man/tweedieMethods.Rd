\name{tweedieReserve methods}
\alias{print.tweedieReserve}
\alias{summary.tweedieReserve}
\title{
Reserve Risk Capital Report
}
\description{
Main purpose of this function is to create a report to assess the Reserve Risk capital given an object of the \code{\link{tweedieReserve}} class. It displays both the 'Ultimate' and '1-yr' risk views at given percentiles.
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
\details{
 [tbd]
}
\value{
 Describe the value returned
  If it is a LIST, use
  \item{Ultimate_View}{Array of 'Ultimate View' Reserve Risk capital at the given percentiles.}
  \item{1yr_View}{Array of '1yr View' Reserve Risk capital at the given percentiles.}
  \item{RC_Ultimate}{Value of 'Ultimate View' Reserve Risk capital at \code{RC.var} level.}
  \item{Diagnostic}{Quick diagnostic to show the deterministic reserve vs Ultimate View and 1yr View best estimate. If the model is working properly, these three value shouldn't be much different.}
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
1+1
}
\keyword{ print }
