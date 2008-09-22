## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:19/09/2008


## source(paste(searchpaths()[grep("ChainLadder", searchpaths())],"/Experimental/BootstrapReserve.R", sep=""))

BootChainLadder <- function(Triangle = RAA, R = 999, process.distr="gamma"){

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


    IBNR.Triangles <- processTriangle
    IBNR <- getLatest(IBNR.Triangles)
    IBNR.Totals <- apply(IBNR.Triangles,3,sum)


    output <- list( call=match.call(expand.dots = FALSE),
                   Triangle=Triangle,
                   IBNR.ByOrigin=IBNR,
                   IBNR.Triangles=IBNR.Triangles,
                   IBNR.Totals = IBNR.Totals,
                   process.distr=process.distr,
                   R=R)

    class(output) <- c("BootChainLadder", class(output))
    return(output)

}
############################################################################
## quantile.BootChainLadder
##
quantile.BootChainLadder <- function(x,probs=c(0.75, 0.99), na.rm = FALSE,
                                     names = TRUE, type = 7,...){

    ByOrigin <- apply(x$IBNR.ByOrigin, 1, quantile, probs=probs,...)
    if(length(probs)>1){
        ByOrigin <- as.data.frame(t(ByOrigin))
    }else{
        ByOrigin <- as.data.frame(ByOrigin)
    }
    names(ByOrigin) <- paste("IBNR ", probs*100, "%", sep="")
    Total.IBNR.q <- quantile(x$IBNR.Totals, probs=probs,...)

    Totals <- as.data.frame(Total.IBNR.q)

    colnames(Totals)=c("Total")
    rownames(Totals) <- paste("IBNR ", probs*100, "%:", sep="")

    output <- list(ByOrigin=ByOrigin, Totals=Totals)

    return(output)
}
############################################################################
## summary.BootChainLadder
##
summary.BootChainLadder <- function(object,probs=c(0.75,0.99),...){

    .Triangle <- object$Triangle
    Latest <- rev(.Triangle[row(as.matrix(.Triangle)) == (nrow(.Triangle)+1 - col(as.matrix(.Triangle)))])
    IBNR <- object$IBNR.ByOrigin
    dim(IBNR) <- dim(IBNR)[c(1,3)]
    IBNR.q <- apply(IBNR, 1, quantile, probs=probs,...)
    IBNR.mean <- apply(IBNR, 1, mean)
    IBNR.sd <- apply(IBNR, 1, sd)
    sumIBNR <- as.data.frame(t(rbind(IBNR.mean, IBNR.sd, IBNR.q)))

    ## ByOrigin
    ByOrigin <- data.frame(Latest, Ult.mean=Latest+sumIBNR$IBNR.mean, sumIBNR)
    names(ByOrigin) <- c("Latest", "Mean Ultimate", "Mean IBNR",
                         "SD IBNR", paste("IBNR ", probs*100, "%", sep=""))
    ex.origin.period <- !is.na(Latest)
    ByOrigin <- ByOrigin[ex.origin.period,]

    ## Totals
    Total.Latest <- sum(Latest,na.rm=TRUE)
    Total.IBNR <- object$IBNR.Totals
    Total.IBNR.mean <-  mean(Total.IBNR)
    Total.IBNR.sd <-  sd(Total.IBNR)
    Total.IBNR.q <- quantile(Total.IBNR, probs=probs,...)

    Totals <-  c(Total.Latest, Total.Latest+Total.IBNR.mean,
                 Total.IBNR.mean, Total.IBNR.sd, Total.IBNR.q)
    Totals <- as.data.frame(Totals)

    colnames(Totals)=c("Total")
    rownames(Totals) <- c("Latest:","Mean Ultimate:",
                          "Mean IBNR:","SD IBNR:",
                          paste("Total IBNR ", probs*100, "%:", sep="") )

    output <- list(ByOrigin=ByOrigin, Totals=Totals)
    return(output)
}
############################################################################
## print.BootChainLadder
##
print.BootChainLadder <- function(x,probs=c(0.75,0.99),...){
    print(x$call)
    cat("\n")
    summary.x <- summary(x,probs=probs)

    print(format(summary.x$ByOrigin, big.mark = ",", digits = 3),...)

    cat("\n")
    Totals <- summary.x$Totals
    print(format(Totals, big.mark=",",digits=3), quote=FALSE)

  }
############################################################################
## plot.BootChainLadder
##
plot.BootChainLadder <- function(x,mfrow=c(1,2),title=NULL,...){

    if(is.null(title)) myoma <- c(0,0,0,0) else myoma <- c(0,0,2,0)

    Total.IBNR <- x$IBNR.Total

    op=par(mfrow=mfrow, oma=myoma,...)
    ## Histogram
    hist(Total.IBNR, xlab="Total IBNR")
    lines(density(Total.IBNR))
    rug(Total.IBNR)
    ## Empirical distribution
    plot(ecdf(Total.IBNR), xlab="Total IBNR")

    title( title , outer=TRUE)
    par(op)
}


##plotBoot <- function(x, probs=c(0.25, 0.5, 0.75,0.99),...){
##    require(lattice)
##    m <- nrow(x$Triangle)
##    n <- ncol(x$Triangle)
##    .Triangle <- x$Triangle
##    IBNR.tria <- apply(x$IBNR,c(1,2), function(.x) c(quantile(.x, probs), mean=mean(.x)))
##    dimnames(IBNR.tria)=list(measurements=dimnames(IBNR.tria)[[1]],
##            origin=dimnames(x$Triangle)[[1]],
##            dev=dimnames(x$Triangle)[[2]])
##    out <- expand.grid(dimnames(IBNR.tria))
##    out$IBNR <- as.vector(IBNR.tria)
##    xyplot(IBNR ~ dev|origin, groups=measurements, as.table=TRUE, t="l", auto.key=TRUE)
##}

makeCumulative <- function(tri){
    ## Author: Nigel de Silva
    tri <- apply(tri, c(1,3), cumsum)
    tri <- aperm(tri, c(2,1,3))
    return(tri)
}

getIncremental <- function(tri){
    ## Author: Nigel de Silva
    .incr <- function(x){
        return(c(x[1], diff(x)))
    }

    out <- apply(tri, c(1,3), .incr)
    out <- aperm(out, c(2,1,3))
    return(out)
}

getLatest <- function(incr.tri){
    ## Author: Nigel de Silva
    out <- apply(incr.tri, c(1,3), sum, na.rm=T)
    dim(out) <- c(1, dim(out))
    out <- aperm(out, c(2,1,3))
    return(out)
}

getIndivDFs <- function(tri){
    ## Author: Nigel de Silva
    .nDevPrds <- dim(tri)[2]
    .numerator <- tri[ , c(2:.nDevPrds, .nDevPrds), , drop=F]
    tri <- .numerator / tri
    tri[ , .nDevPrds, ] <- NA
    return(tri)
}

getAvDFs <- function(dfs, wghts){
    ## Author: Nigel de Silva
    .include <- dfs
    .include[!is.na(.include)] <- 1
    .include[is.na(wghts)] <- NA

    out <- apply(dfs * wghts * .include, c(2,3), sum, na.rm=T) /
      apply(wghts * .include, c(2,3), sum, na.rm=T)

    out[is.nan(out)] <- 1


  out <- array(out, c(1,dim(out)))
  return(out)
}

getUltDFs <- function(avDFs){
    ## Author: Nigel de Silva
    .ultDF<- function(x){
        return(rev(cumprod(rev(x))))
    }

    ult <- apply(avDFs, c(1,3), .ultDF)
    ult <- aperm(ult, c(2,1,3))
    return(ult)
}

getUltimates <- function(latest, ultDFs){
    ## Author: Nigel de Silva
    ultDFs <- apply(ultDFs, c(1,3), rev)
    out <- latest * ultDFs
    return(out)
}

expandArray <- function(x, d, new.size){
    ## Author: Nigel de Silva
    ## Expand array x, in d th dimension to new.size
    ## Permute dimension to set changing dimension last
  .n <- length(dim(x))
  .perm <- 1:.n
  .perm[d] <- .n
  .perm[.n] <- d
  x <- aperm(x, .perm)

  ## Expand dimension
  x <- array(x, dim=c(dim(x)[-.n], new.size))

  ##Return to old dimension arrangement
  x <- aperm(x, .perm)

  return(x)

}

getExpected <- function(ults, ultDFs){
    ## Author: Nigel de Silva
    ## get backward chain ladder incremental triangles
    ults <- expandArray(ults, 2, dim(ultDFs)[2])
    ultDFs <- expandArray(ultDFs, 1, dim(ults)[1])

    return(ults * ultDFs)
}

sampleResiduals <- function(resids, positions, n.sims){
    ## Author: Nigel de Silva
    ## Worry about excluding, zoning, etc. residuals later

    resids <- as.vector(resids)
    resids <- resids[!is.na(resids)]

    .sample <- sample(resids, prod(dim(positions)[-3])*n.sims, replace=T)
    .sample <- array(.sample, dim=c(dim(positions)[-3], n.sims))
    .sample[is.na(positions)] <- NA

    return(.sample)
}

randomClaims <- function(exp.clms, resids, n.sims){
    ## Author: Nigel de Silva
    .residSample <- sampleResiduals(resids, exp.clms, n.sims)
    exp.clms <- expandArray(exp.clms, 3, n.sims)
    out <- .residSample * sqrt(abs(exp.clms)) + exp.clms

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
