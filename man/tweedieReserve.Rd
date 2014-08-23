\name{tweedieReserve}
\alias{tweedieReserve}
\title{
Tweedie Stochastic Reserving Model
}
\description{
This function implements loss reserving models within the generalized linear model framework in order to generate the full predictive distribution for loss reserves. Besides, it generates also the "1-yr Risk View" useful to derive the Reserve Risk Capital in a Solvency 2 framework. Finally, it gives to the user the opportunity to properly assess the Model Error changing different model parameters.
}
\usage{
tweedieReserve(triangle, var.power = 1, 
                    link.power = 0, design.type = c(1, 1, 0), 
                    rereserving = FALSE, cum = TRUE, exposure = FALSE, 
                    bootstrap = 1, boot.adj = 0, nsim = 1000, 
                    proc.err = TRUE, p.optim = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{triangle}{
     An object of  class \code{\link{triangle}}.
}
  \item{var.power}{
    The index (p) of the power variance function \eqn{V(\mu)=\mu^p}. Default to \code{p = 1}, which is the over-dispersed Poisson model. If \code{NULL}, it will be assumed to be in \code{(1, 2)} and estimated using the \code{cplm} package.  See \code{\link[statmod]{tweedie}}.
}
  \item{link.power}{
  The index of power link function. The default \code{link.power = 0} produces a log link. See \code{\link[statmod]{tweedie}}.
}
  \item{design.type}{
  It's a 3 dimension array that specifies the design matrix underlying the GLM. The dimensions represent respectively: Origin Year, Development Year and Calendar Year. Accepted values are: \code{0} (not modeled), \code{1} (modeled as factor) and \code{2} (modeled as variable). Default to \code{c(1,1,0)}, which is the common specification in actuarial literature (Origin and Development Year as factors, Calendar Year not modeled).
}
  \item{rereserving}{
  Boolean, if \code{TRUE} the 1-yr risk view loss reserve distribution is derived. Default to \code{FALSE} (ATTENTION: it could materially increase run-time).
}
  \item{cum}{
  Boolean, indicating whether the input triangle is on the
  cumulative or the incremental scale. If \code{TRUE}, then
  \code{triangle} is assumed to be on the cumulative scale, and it will
  be converted to incremental losses internally before a GLM is fitted. 
}
  \item{exposure}{
  Boolean, if \code{TRUE} the \code{exposure} defined in the \code{\link{triangle}} object is specified as \code{offset} in the GLM model specification. Default to \code{FALSE}.
}
  \item{bootstrap}{
  Integer, it specifies the type of bootstrap for parameter error. Accepted values are: \code{0} (disabled), \code{1} (parametric), \code{2} (semi-parametric). Default to \code{1}.
}
  \item{boot.adj}{
  Integer, it specified the methodology when using semi-parametric bootstrapping. Accepted values are: \code{0} (cycles until all the values of the pseudo-triangle are >= 0), \code{1} (overwrite negative values to 0.01). Default to \code{0} (ATTENTION: it could materially increase run-time when semi-parametric bootstrap is used)
}
  \item{nsim}{
  Integer, number of simulations to derive the loss reserve distribution. Default to \code{1000} (ATTENTION: high num of simulations could materially increase run-time, in particular if a re-reserving alghorithm is used as well)
}
  \item{proc.err}{
  Boolean, if \code{TRUE} a process error (coherent with the specified model) is added to the forecasted distribution. Default to \code{TRUE}.
}
  \item{p.optim}{
  Boolean, if \code{TRUE} the model estimates the MLE for the Tweedie's \code{p} parameter. Default to \code{FALSE}. Recommended to use at the beginning of the Stochastic Reserving Analysis.
}
  \item{\dots}{
  Arguments to be passed onto the function \code{\link[stats]{glm}} or \code{cpglm} such as \code{contrasts} or \code{control}. It is important that \code{offset} and \code{weight} should not be specified. Otherwise, an error will be reported and the program will quit.
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
res <- tweedieReserve(UKMotor, rereserving=TRUE)
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
