\name{qpaid}
\alias{qpaid}
\alias{qincurred}
\docType{data}
\title{Quarterly run off triangle of accumulated claims data}
\description{}
  
\usage{data(qpaid); data(qincurred)}
\format{
  A matrix with 12 accident years and 45 development quarters of claims developments.
}
\source{Made up data}
\examples{
qpaid
qincurred
op=par(mfrow=c(1,2))
ymax <- max(c(qpaid,qincurred),na.rm=TRUE)*1.05
matplot(t(qpaid), type="l", main="Paid development", xlab="Dev. quarter", ylab="$", ylim=c(0,ymax))
matplot(t(qincurred), type="l", main="Incurred development", xlab="Dev. quarter", ylab="$", ylim=c(0,ymax))
par(op)
# MackChainLadder expects a quadratic matrix so let's expand the triangle to a quarterly origin period.
n <- ncol(qpaid)
Paid <- matrix(NA, n, n)
Paid[seq(1,n,4),] <- qpaid
M <- MackChainLadder(Paid)
plot(M)

Incurred <- matrix(NA, n, n)
Incurred[seq(1,n,4),] <- qincurred

MunichChainLadder(Paid, Incurred)

}
\keyword{datasets}
