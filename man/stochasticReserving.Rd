\name{stochasticReserving}
\alias{stochasticReserving}
\title{
Ale please provide a title here
}
\description{
A concise (1-5 lines) description of what the function does.
}
\usage{
stochasticReserving(triangle, var.power = 1, 
                    link.power = 0, design.type = c(1, 1, 0), 
                    rereserving = FALSE, cum = TRUE, exposure = FALSE, 
                    bootstrap = 1, boot.adj = 0, nsim = 1000, 
                    proc.err = TRUE, p.optim = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{triangle}{
     Describe \code{triangle} here
}
  \item{var.power}{
      Describe \code{var.power} here
}
  \item{link.power}{
     Describe \code{link.power} here
}
  \item{design.type}{
     Describe \code{design.type} here
}
  \item{rereserving}{
     Describe \code{rereserving} here
}
  \item{cum}{
     Describe \code{cum} here
}
  \item{exposure}{
     Describe \code{exposure} here
}
  \item{bootstrap}{
    Describe \code{bootstrap} here
}
  \item{boot.adj}{
     Describe \code{boot.adj} here
}
  \item{nsim}{
     Describe \code{nsim} here
}
  \item{proc.err}{
    Describe \code{proc.err} here
}
  \item{p.optim}{
     Describe \code{p.optim} here
}
  \item{\dots}{
     Describe \code{\dots} here
}
}
\details{
 If necessary, more details than the description above 
}
\value{
 Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}

}
\references{
 Put references to the literature/web site here
}
\author{
 Who to complain to
}
\note{
Further notes
}

\section{Warning }{Make other sections like Warning}

\seealso{
 See Also as \code{\link{help}}
}
\examples{
## R code of your examples
## R code of your examples
res <- stochasticReserving(UKMotor, rereserving=TRUE)
RC_report(res)
## One year reserving risk 
RC_report(res)$RC_1yr

## Alternative apporach using bootstrap chain ladder
## Use bootstrap and chainladder to estimate one year reserve risk
M <- MackChainLadder(UKMotor, est.sigma="Mack")
# Chain ladder link ratios
f <- M$f
# Latest triangle
latest <- summary(M)$ByOrigin$Latest
# Bootstrap chain ladder
set.seed(1) # set seed to have a replicatable example
B <- BootChainLadder(UKMotor, R=1000, process.distr="od.pois")


# Get next year indices
n <- ncol(UKMotor)
ny <- (col(UKMotor) == (nrow(UKMotor) - row(UKMotor) + 2))
# Extract the 1 in 200 triangle
triangle.ny <- apply(B$IBNR.Triangles, 3, 
                 function(x){
                   next.year <- x[col(x) == (nrow(x) - row(x) + 2)]
                   sum(next.year)
                 })
triangle.ny.995 <- B$IBNR.Triangles[,,order(triangle.ny)[round(B$R*0.995)]]
# Create a triangle with additional 1 in 200 diagonal from the Bootstrap
UKMotor.ny <- cum2incr(UKMotor)
UKMotor.ny[ny] <- triangle.ny.995[ny]
# Apply chain ladder again
(UKMotor.ny <- t(apply(UKMotor.ny,1,cumsum)))
f.ny <- sapply((n-1):1, function(i){
  sum(UKMotor.ny[1:(i+1), n-i+1])/sum(UKMotor.ny[1:(i+1), n-i])
})
(f.ny <- c(f.ny[-(n-1)],1))
full.triangle.ny <- UKMotor.ny
for(k in 2:(n-1)){ 
  full.triangle.ny[(n-k+2):n, k+1] <- full.triangle.ny[(n-k+2):n,k]*f[k]
}
## Re-reserving 1 in 200 IBNR
(sum(re.reserve.995 <- full.triangle.ny[,n] - rev(latest)))
# Compare to the glm approach
RC_report(res)$RC_1yr
 
}
\keyword{ models }
