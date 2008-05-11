

BootReserve <- function(triangle = RAA, R = 1000, process.distr="gamma"){



# Obtain the standard chain-ladder development factors from cumulative data.


n <- ncol(triangle)
triangle <- array(triangle, dim=c(n,n,1))
inc.triangle <- getIncremental(triangle)

lobs <- row(triangle[,,1]) == (n+1 - col(triangle[,,1]))
obs <- row(triangle[,,1]) <= (n+1 - col(triangle[,,1]))
Latest <- getLatest(inc.triangle)

# Obtain cumulative fitted values for the past triangle by backwards
# recursion, starting with the observed cumulative paid to date in the latest
# diagonal

dfs <- getIndivDFs(triangle)
weights <- triangle
avDFs <- getAvDFs(dfs, weights)
ultDFs <- getUltDFs(avDFs)
ults <- getUltimates(Latest, ultDFs)
# Obtain incremental fitted values, m^ ij, for the past triangle by differencing.
exp.inc.triangle <- getIncremental(getExpected(ults, 1/ultDFs))
exp.inc.triangle[is.na(inc.triangle[,,1])] <- NA


# Calculate the unscaled Pearson residuals for the past triangle using:
unscaled.residuals  <- (inc.triangle - exp.inc.triangle)/sqrt(abs(exp.inc.triangle))

# Calculate the Pearson scale parameter
nobs  <- sum(1:n)
scale.factor <- (0.5*n*(n+1)-2*n+1)
scale.phi <- sum(unscaled.residuals^2,na.rm=TRUE)/scale.factor
# Adjust the Pearson residuals using
adj.resids <- unscaled.residuals * sqrt(nobs/scale.factor)


# Sample incremental claims
# Resample the adjusted residuals with replacement, creating a new
# past triangle of residuals.

simClaims <- randomClaims(exp.inc.triangle, adj.resids, R)

# Fit the standard chain-ladder model to the pseudo-cumulative data.
# Project to form a future triangle of cumulative payments.

# Perform chain ladder projection
simLatest <- getLatest(simClaims)
simCum <- makeCumulative(simClaims)
simDFs <- getIndivDFs(simCum)
simWghts <- simCum
simAvDFs <- getAvDFs(simDFs, simWghts)
simUltDFs <- getUltDFs(simAvDFs)
simUlts <- getUltimates(simLatest, simUltDFs)
# Get expected future claims
# Obtain the corresponding future triangle of incremental payments by
# differencing, to be used as the mean when simulating from the process
# distribution.

simExp <- getIncremental(getExpected(simUlts, 1/simUltDFs))
simExp[!is.na(simClaims)] <- NA

if(process.distr=="gamma")
  processTriangle <-  apply(simExp,c(1,2,3), function(x)
          ifelse(is.na(x), NA, sign(x)*rgamma(1, shape=abs(x/scale.phi), scale=scale.phi)))
if(process.distr=="od.pois")
  processTriangle <-  apply(simExp,c(1,2,3), function(x)
          ifelse(is.na(x), NA, sign(x)*rpois.od(1, abs(x), scale.phi)))
#if(process.distr=="od.nbionm")
#  processTriangle <-  apply(simExp,c(1,2,3), function(x)
#          ifelse(is.na(x), NA, sign(x)*rnbinom.od(1, mu=abs(x), size=sqrt(1+abs(x)),d=scale.phi)))


processTriangle[is.na(processTriangle)] <- 0


IBNR <- processTriangle #apply(getLatest(gammaTriangle),3,sum)

class(IBNR) <- c("BootReserve", class(IBNR))
return(IBNR)

}
 
summary.BootReserve <- function(object,probs=c(0.25,0.5,0.75,0.9,0.995),...){

 IBNR <- getLatest(object)
 dim(IBNR) <- dim(IBNR)[c(1,3)]
 IBNR.q <- apply(IBNR, 1, quantile, probs)
 IBNR.mean <- apply(IBNR, 1, mean)
 IBNR.sd <- apply(IBNR, 1, sd)
# Total.IBNR <-  apply(getLatest(object),3,sum)
  sumIBNR <- as.data.frame(t(rbind(IBNR.mean, IBNR.sd, IBNR.q)))
  
  return(sumIBNR)


}

print.BootReserve <- function(x,...){

 print(summary(x))
 

}


makeCumulative <- function(tri)
{
  tri <- apply(tri, c(1,3), cumsum)
  tri <- aperm(tri, c(2,1,3))
  return(tri)
}

getIncremental <- function(tri)
{
  .incr <- function(x)
  {
    return(c(x[1], diff(x)))
  }

  out <- apply(tri, c(1,3), .incr)
  out <- aperm(out, c(2,1,3))
  return(out)
}

getLatest <- function(incr.tri)
{
  out <- apply(incr.tri, c(1,3), sum, na.rm=T)
  dim(out) <- c(1, dim(out))
  out <- aperm(out, c(2,1,3))
  return(out)
}

getIndivDFs <- function(tri)
{
  .nDevPrds <- dim(tri)[2]
  .numerator <- tri[ , c(2:.nDevPrds, .nDevPrds), , drop=F]
  tri <- .numerator / tri
  tri[ , .nDevPrds, ] <- NA
  return(tri)
}

getAvDFs <- function(dfs, wghts)
{
  .include <- dfs
  .include[!is.na(.include)] <- 1
  .include[is.na(wghts)] <- NA

  out <- apply(dfs * wghts * .include, c(2,3), sum, na.rm=T) /
    apply(wghts * .include, c(2,3), sum, na.rm=T)

  out[is.nan(out)] <- 1


  out <- array(out, c(1,dim(out)))
  return(out)
}

getUltDFs <- function(avDFs)
{
  .ultDF<- function(x)
  {
    return(rev(cumprod(rev(x))))
  }

  ult <- apply(avDFs, c(1,3), .ultDF)
  ult <- aperm(ult, c(2,1,3))
  return(ult)
}

getUltimates <- function(latest, ultDFs)
{
  ultDFs <- apply(ultDFs, c(1,3), rev)
  out <- latest * ultDFs
  return(out)
}

expandArray <- function(x, d, new.size)
{
  # Expand array x, in d th dimension to new.size

  # Permute dimension to set changing dimension last
  .n <- length(dim(x))
  .perm <- 1:.n
  .perm[d] <- .n
  .perm[.n] <- d
  x <- aperm(x, .perm)

  # Expand dimension
  x <- array(x, dim=c(dim(x)[-.n], new.size))

  #Return to old dimension arrangement
  x <- aperm(x, .perm)

  return(x)

}

getExpected <- function(ults, ultDFs)
{
  # get backward chain ladder incremental triangles
  ults <- expandArray(ults, 2, dim(ultDFs)[2])
  ultDFs <- expandArray(ultDFs, 1, dim(ults)[1])

  return(ults * ultDFs)
}

sampleResiduals <- function(resids, positions, n.sims)
{
  # Worry about excluding, zoning, etc. residuals later

  resids <- as.vector(resids)
  resids <- resids[!is.na(resids)]

  .sample <- sample(resids, prod(dim(positions)[-3])*n.sims, replace=T)
  .sample <- array(.sample, dim=c(dim(positions)[-3], n.sims))
  .sample[is.na(positions)] <- NA

  return(.sample)
}

randomClaims <- function(exp.clms, resids, n.sims)
{
  .residSample <- sampleResiduals(resids, exp.clms, n.sims)
  exp.clms <- expandArray(exp.clms, 3, n.sims)
  out <- .residSample * sqrt(exp.clms) + exp.clms

  return(out)
}

# Mike Lonergan mel at mcs.st-and.ac.uk
# Fri Jun 14 19:31:24 CEST 2002
# functions to generate random numbers following
# over-dispersed Poisson and negative binomial distributions
# based on simple cheat of using a standard negative binomial,
# but choosing the scale parameter to give the desired mean/variance
# ratio at the given value of the mean.

rpois.od<-function (n, lambda,d=1) {
    if (d==1)
       rpois(n, lambda)
    else
       rnbinom(n, size=(lambda/(d-1)), mu=lambda)
}

rnbinom.od<-function (n, size, prob, mu, d=1) {
    if (!missing(prob)) {
       if (!missing(mu))
          stop("prob and mu both specified")
       mu<-size*(1-prob)/prob
    }
    size2 <- mu/(d-1+(d*mu/size))
    prob2 <- size2/(size2 + mu)
    .Internal(rnbinom(n, size2, prob2))
}
