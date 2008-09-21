\name{BootReserve}
\alias{BootReserve}
\alias{print.BootReserve}
\alias{plot.BootReserve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BootReserve(Triangle = RAA, R = 999, process.distr = "gamma")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{ ~~Describe \code{Triangle} here~~ }
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{process.distr}{ ~~Describe \code{process.distr} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(Triangle = RAA, R = 999, process.distr="gamma"){

    triangle <- Triangle
    if(nrow(triangle) != ncol(triangle))
 	stop("Number of origin years has to be equal to number of development years.\n")

    ## Obtain the standard chain-ladder development factors from cumulative data.

    n <- ncol(triangle)
    triangle <- array(triangle, dim=c(n,n,1))
    inc.triangle <- getIncremental(triangle)

    lobs <- row(triangle[,,1]) == (n+1 - col(triangle[,,1]))
    obs <- row(triangle[,,1]) <= (n+1 - col(triangle[,,1]))
    Latest <- getLatest(inc.triangle)

    ## Obtain cumulative fitted values for the past triangle by backwards
    ## recursion, starting with the observed cumulative paid to date in the latest
    ## diagonal

    dfs <- getIndivDFs(triangle)
    weights <- triangle
    avDFs <- getAvDFs(dfs, weights)
    ultDFs <- getUltDFs(avDFs)
    ults <- getUltimates(Latest, ultDFs)
    ## Obtain incremental fitted values, m^ ij, for the past triangle by differencing.
    exp.inc.triangle <- getIncremental(getExpected(ults, 1/ultDFs))
    exp.inc.triangle[is.na(inc.triangle[,,1])] <- NA

    ## Calculate the unscaled Pearson residuals for the past triangle using:
    unscaled.residuals  <- (inc.triangle - exp.inc.triangle)/sqrt(abs(exp.inc.triangle))

    ## Calculate the Pearson scale parameter
    nobs  <- sum(1:n)
    scale.factor <- (0.5*n*(n+1)-2*n+1)
    scale.phi <- sum(unscaled.residuals^2,na.rm=TRUE)/scale.factor
    ## Adjust the Pearson residuals using
    adj.resids <- unscaled.residuals * sqrt(nobs/scale.factor)


    ## Sample incremental claims
    ## Resample the adjusted residuals with replacement, creating a new
    ## past triangle of residuals.

    simClaims <- randomClaims(exp.inc.triangle, adj.resids, R)

    ## Fit the standard chain-ladder model to the pseudo-cumulative data.
    ## Project to form a future triangle of cumulative payments.

    ## Perform chain ladder projection
    simLatest <- getLatest(simClaims)
    simCum <- makeCumulative(simClaims)
    simDFs <- getIndivDFs(simCum)
    simWghts <- simCum
    simAvDFs <- getAvDFs(simDFs, simWghts)
    simUltDFs <- getUltDFs(simAvDFs)
    simUlts <- getUltimates(simLatest, simUltDFs)
    ## Get expected future claims
    ## Obtain the corresponding future triangle of incremental payments by
    ## differencing, to be used as the mean when simulating from the process
    ## distribution.

    simExp <- getIncremental(getExpected(simUlts, 1/simUltDFs))
    simExp[!is.na(simClaims)] <- NA

    if(process.distr=="gamma")
        processTriangle <-  apply(simExp,c(1,2,3), function(x)
                                  ifelse(is.na(x), NA, sign(x)*rgamma(1, shape=abs(x/scale.phi), scale=scale.phi)))
    if(process.distr=="od.pois")
        processTriangle <-  apply(simExp,c(1,2,3), function(x)
                                  ifelse(is.na(x), NA, sign(x)*rpois.od(1, abs(x), scale.phi)))
    ##if(process.distr=="od.nbionm")
    ##  processTriangle <-  apply(simExp,c(1,2,3), function(x)
    ##          ifelse(is.na(x), NA, sign(x)*rnbinom.od(1, mu=abs(x), size=sqrt(1+abs(x)),d=scale.phi)))


    processTriangle[is.na(processTriangle)] <- 0


    IBNR <- processTriangle #apply(getLatest(gammaTriangle),3,sum)

    output <- list( call=match.call(expand.dots = FALSE),
                   Triangle=Triangle,
                   IBNR=IBNR,
                   R=R)

    class(output) <- c("BootReserve", class(output))
    return(output)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
