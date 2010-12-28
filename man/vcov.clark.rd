\name{vcov.clark}
\alias{vcov.clark}
\title{Covariance Matrix of Parameter Estimates -- Clark's methods}
\description{
Function to compute the covariance matrix of the parameter estimates
for the ClarkLDF and ClarkCapeCod methods.
}
\usage{
\method{vcov}{clark}(object, \dots)
}
\arguments{
\item{object}{
object resulting from a run of the ClarkLDF or ClarkCapeCod functions.
}
\item{\dots}{
not used.
}
}
\details{
The covariance matrix of the estimated parameters is estimated
by the inverse of the Information matrix (see Clark, p. 53).
This function uses the "FI" and "sigma2" values returned by 
ClarkLDF and by ClarkCapeCod and calculates the matrix
\cr
-sigma2*FI^-1.
}
\references{
Clark, David R., 
"LDF Curve-Fitting and Stochastic Reserving: A Maximum Likelihood Approach",
\emph{Casualty Actuarial Society Forum}, Fall, 2003
}
\author{
Daniel Murphy
}
\seealso{
\code{\link{ClarkLDF}}, \code{\link{ClarkCapeCod}}
}
\examples{

x <- GenIns
colnames(x) <- 12*as.numeric(colnames(x))
Y <- ClarkCapeCod(x, Premium=10000000+400000*0:9, maxage=240)
round(vcov(Y),6) ## Compare to matrix on p. 69 of Clark's paper

# The estimates of the loglogistic parameters
tail(Y$par, 2)
# The standard errors of the estimated parameters
sqrt(tail(diag(vcov(Y)), 2))

# The parameter risks of the estimated reserves are calculated 
# according to the formula on p. 54 of Clark's paper. For example, for
# the 5th accident year, pre- and post-multiply the covariance matrix
# by a matrix consisting of the gradient entries for just that accident year
dR5 <- matrix(Y$dR[, 5], ncol=1)
sqrt(t(dR5) \%*\% vcov(Y) \%*\% dR5) ## compares to 314,829 in Clark's paper

# The estimated reserves for accident year 5:
Y$Table65$EstimatedReserves[5]   ## compares to 2,046,646 in the paper

# The parameter risk CV for all accident years in total (10.6% in paper):
sqrt(sum(t(Y$dR) \%*\% vcov(Y) \%*\% Y$dR)) / Y$Table65$EstimatedReserves[11]

}

