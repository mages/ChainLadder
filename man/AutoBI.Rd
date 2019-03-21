\name{AutoBI}
\alias{AutoBI}
\docType{data}
\title{Run off triangles of accumulated claim data}
\description{Run-off triangles of Automobile Bodily Injury Liability.}

\usage{data(AutoBI)}
\format{
  Portfolio of automobile bodily injury liability for an experience period of 1969 to 1976. Paid Claims, Closed Claims and Reported Claim Counts respectively
}
\source{
	\cite{Berquist, J.R. and Sherman, R.E., Loss Reserve Adequacy Testing: A Comprehensive,
  Systematic Approach, \emph{Proceedings of the Casualty Actuarial Society}, LXIV, 1977, pp.123-184.}
}
\examples{
data(AutoBI)
names(AutoBI)
AutoBI$AutoBIPaid
AutoBI$AutoBIClosed
AutoBI$AutoBIReportedCounts
}
\keyword{datasets}