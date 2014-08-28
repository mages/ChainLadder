\name{RC_report}
\alias{RC_report}
\title{
Reserve Risk Capital Report
}
\description{
Main purpose of this function is to create a report to assess the Reserve Risk capital given an object of the \code{\link{tweedieReserve}} class. It displays both the "Ultimate" and "1-yr" risk views at given percentiles.
}
\usage{
RC_report(res, q = c(0.4, 0.5, 0.7, 0.9, 0.95, 0.995, 0.9993), RC.var = 0.995)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
    An object of class \code{\link{tweedieReserve}}.
}
  \item{q}{
    Array of percentiles to be displayed. Default to \code{c(0.4, 0.5, 0.7, 0.9, 0.95, 0.995, 0.9993)}
}
  \item{RC.var}{
    Risk Capital percentile (using the Value at Risk - VaR - as volume measure)
}
}
\details{
 [tbd]
}
\value{
 Describe the value returned
  If it is a LIST, use
  \item{Ultimate_View}{Array of "Ultimate View" Reserve Risk Capital at the given percentiles.}
  \item{1yr_View}{Array of "1yr View" Reserve Risk Capital at the given percentiles.}
  \item{RC_Ultimate}{Value of Ultimate View Reserve Risk capital at \code{RC.var} level.}
  \item{RC_1yr}{Value of Ultimate View Reserve Risk capital at \code{RC.var} level.}
  \item{Diagnostic}{Quick diagnostic to show the deterministic reserve vs Ultimate View and 1yr View best estimate. If the model is working properly, these three value shouldn't be much different.}
}
\references{
 n/a
}
\author{
 Alessandro Carrato MSc FIA OA \email{alessandro.carrato@gmail.com}
}
\note{
Further notes
}

<<<<<<< HEAD
\section{Warning}{
  [tbd]
}
=======
\section{Warning}
{Make other sections like Warning}
>>>>>>> 52f1263e8d78e8bec2ed8be0db303bcc0d8866c7

\seealso{
 See also \code{\link{tweedieReserve}}.
}
\examples{
[tbd]
}
\keyword{ print }
