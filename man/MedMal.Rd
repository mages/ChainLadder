\name{MedMal}
\alias{MedMal}
\docType{data}
\title{Run off triangles of accumulated claim data}
\description{Run-off triangles of Medical Malpractice Data insurance.}

\usage{data(MedMal)}
\format{
  U.S. medical malpractice insurance for an experience period of 1969 to 1976. Reported Claims, Paid Claims, Case Outstanding and Open Claims respectively
}
\source{
	\cite{Berquist, J.R. and Sherman, R.E., Loss Reserve Adequacy Testing: A Comprehensive,
  Systematic Approach, \emph{Proceedings of the Casualty Actuarial Society}, LXIV, 1977, pp.123-184.}
}
\examples{
data(MedMal)
names(MedMal)
MedMal$MedMalReported
MedMal$MedMalPaid
MedMal$MedMalOutstanding
MedMal$MedMalOpen
}
\keyword{datasets}