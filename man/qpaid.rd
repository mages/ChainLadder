\name{qpaid}
\alias{qpaid}
\alias{qincurred}
\docType{data}
\title{Quarterly run off triangle of accumulated claims data}
\description{Sample data to demonstrate how to work with triangles with a higher development period frequency than origin period frequency}
  
\usage{data(qpaid); data(qincurred)}
\format{
  A matrix with 12 accident years and 45 development quarters of claims costs.
}
\source{Made up data for testing purpose}
\examples{
dim(qpaid)
dim(qincurred)
op=par(mfrow=c(1,2))
ymax <- max(c(qpaid,qincurred),na.rm=TRUE)*1.05
matplot(t(qpaid), type="l", main="Paid development", 
		  xlab="Dev. quarter", ylab="$", ylim=c(0,ymax))
matplot(t(qincurred), type="l", main="Incurred development", 
		      xlab="Dev. quarter", ylab="$", ylim=c(0,ymax))
par(op)
## MackChainLadder expects a quadratic matrix so let's expand 
## the triangle to a quarterly origin period.
n <- ncol(qpaid)
Paid <- matrix(NA, n, n)
Paid[seq(1,n,4),] <- qpaid
M <- MackChainLadder(Paid)
plot(M)

# We expand the incurred triangle in the same way 
Incurred <- matrix(NA, n, n)
Incurred[seq(1,n,4),] <- qincurred

# With the expanded triangles we can apply MunichChainLadder
MunichChainLadder(Paid, Incurred)

# In the same way we can apply BootChainLadder
# We reduce the resample size R from the default of 999 to 99 in this example purely to reduce run time.
BootChainLadder(Paid, R=99) 

}
\keyword{datasets}
