## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:19/10/2008

BootChainLadder <- function(Triangle, R = 999, process.distr=c("gamma", "od.pois")){

  
  if(!'matrix' %in% class(Triangle))
    Triangle <- as.matrix(Triangle)
  
  process.distr <- match.arg(process.distr)
  weights <- 1/Triangle
  
  triangle <- Triangle
  if(nrow(triangle) < ncol(triangle))
    stop("Number of origin periods has to be equal or greater then the number of development periods.\n")
  
  triangle <- checkTriangle(Triangle)
  output.triangle <- triangle
  
  m <- dim(triangle)[1]
  n <- dim(triangle)[2]
  origins <- c((m-n+1):m)
  
  ## Obtain the standard chain-ladder development factors from cumulative data.

  triangle <- array(triangle, dim=c(m,n,1))
  weights <-  array(weights, dim=c(m,n,1))
  inc.triangle <- getIncremental(triangle)
  Latest <- getLatest(inc.triangle)[origins]
  
  ## Obtain cumulative fitted values for the past triangle by backwards
  ## recursion, starting with the observed cumulative paid to date in the latest
  ## diagonal
  
  dfs <- getIndivDFs(triangle)
  avDFs <- getAvDFs(dfs, 1/weights)
  ultDFs <- getUltDFs(avDFs)
  ults <- getUltimates(Latest, ultDFs)
  ## Obtain incremental fitted values, m^ ij, for the past triangle by differencing.
  exp.inc.triangle <- getIncremental(getExpected(ults, 1/ultDFs))
  ## exp.inc.triangle[is.na(inc.triangle[,,1])] <- NA
  exp.inc.triangle[is.na(inc.triangle[origins,,1])] <- NA
  ## Calculate the unscaled Pearson residuals for the past triangle using:
  inc.triangle <- inc.triangle[origins,,]
  dim(inc.triangle) <- c(n,n,1)
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
  if(m>n){
    IBNR <- apply(IBNR, 3, function(x) c(rep(0, m-n),x))
    dim(IBNR) <- c(m,1,R)
    IBNR.Triangles <- apply(IBNR.Triangles,3, function(x) rbind(matrix(0,m-n,n),x))
    dim(IBNR.Triangles) <- c(m,n,R)
    
    simClaims <- apply(simClaims,3, function(x) rbind(matrix(0,m-n,n),x))
    dim(simClaims) <- c(m,n,R)
  }
  
  IBNR.Totals <- apply(IBNR.Triangles,3,sum)
  
  residuals <- adj.resids
  
  output <- list( call=match.call(expand.dots = FALSE),
                 Triangle=output.triangle,
                 f=as.vector(avDFs),
                 simClaims=simClaims,
                 IBNR.ByOrigin=IBNR,
                 IBNR.Triangles=IBNR.Triangles,
                 IBNR.Totals = IBNR.Totals,
                 ChainLadder.Residuals=residuals,
                 process.distr=process.distr,
                 R=R)
  
  class(output) <- c("BootChainLadder", class(output))
  return(output)
  
}

############################################################################
## residuals.BootChainLadder
##

residuals.BootChainLadder <- function(object,...){
    return(object$ChainLadder.Residuals)
}
############################################################################
## quantile.BootChainLadder
##
quantile.BootChainLadder <- function(x,probs=c(0.75, 0.95), na.rm = FALSE,
                                     names = TRUE, type = 7,...){

    m <- dim(x$Triangle)[1]
    n <- dim(x$Triangle)[2]

    ByOrigin <- t(apply(x$IBNR.ByOrigin, 1, quantile, probs=probs, names=names, type=type,...))

    if(length(probs)>1){
        ByOrigin <- as.data.frame(ByOrigin)
    }else{
        ByOrigin <- as.data.frame(t(ByOrigin))
    }
    names(ByOrigin) <- paste("IBNR ", probs*100, "%", sep="")

    origin <- dimnames(x$Triangle)[[1]]

    if(length(origin)==nrow(ByOrigin)){
        rownames(ByOrigin) <- origin
    }


    Total.IBNR.q <- quantile(x$IBNR.Totals, probs=probs, names=names, type=type, ...)

    Totals <- as.data.frame(Total.IBNR.q)

    colnames(Totals)=c("Totals")
    rownames(Totals) <- paste("IBNR ", probs*100, "%:", sep="")

    output <- list(ByOrigin=ByOrigin, Totals=Totals)

    return(output)
}
############################################################################
## mean.BootChainLadder
##
mean.BootChainLadder <- function(x,...){

    m <- dim(x$Triangle)[1]
    n <- dim(x$Triangle)[2]

    ByOrigin <- apply(x$IBNR.ByOrigin, 1, mean,...)

    ByOrigin <- as.data.frame(ByOrigin)

    names(ByOrigin) <- "Mean IBNR"

    origin <- dimnames(x$Triangle)[[1]]
    if(length(origin)==nrow(ByOrigin)){
        rownames(ByOrigin) <- origin
    }

    Total.IBNR <- mean(x$IBNR.Totals,...)
    Totals <- as.data.frame(Total.IBNR)

    colnames(Totals)=c("Total")
    rownames(Totals) <- "Mean IBNR:"

    output <- list(ByOrigin=ByOrigin, Totals=Totals)

    return(output)
}


############################################################################
## summary.BootChainLadder
##
summary.BootChainLadder <- function(object,probs=c(0.75,0.95),...){

    .Triangle <- object$Triangle
    m <- dim(.Triangle)[1]
    n <- dim(.Triangle)[2]

    dim(.Triangle) <- c(dim(.Triangle),1)
    Latest <- as.vector(getLatest(getIncremental(.Triangle)))


    IBNR <- object$IBNR.ByOrigin
    dim(IBNR) <- dim(IBNR)[c(1,3)]
    IBNR.q <- apply(IBNR, 1, quantile, probs=probs,...)
    IBNR.mean <- apply(IBNR, 1, mean)
    IBNR.sd <- apply(IBNR, 1, sd)
    sumIBNR <- t(rbind(IBNR.mean, IBNR.sd, IBNR.q))
    sumIBNR <- as.data.frame(sumIBNR)

    ## ByOrigin
    ByOrigin <- data.frame(Latest,
                           Ult.mean=Latest+sumIBNR$IBNR.mean,
                           sumIBNR)
    names(ByOrigin) <- c("Latest", "Mean Ultimate", "Mean IBNR",
                         "SD IBNR", paste("IBNR ", probs*100, "%", sep=""))
    ex.origin.period <- Latest!=0
    ByOrigin <- ByOrigin[ex.origin.period,]

    origin <- dimnames(object$Triangle)[[1]]
    if(length(origin)==nrow(ByOrigin)){
        rownames(ByOrigin) <- origin
    }

    ## Totals
    Total.Latest <- sum(Latest,na.rm=TRUE)
    Total.IBNR <- object$IBNR.Totals
    Total.IBNR.mean <-  mean(Total.IBNR)
    Total.IBNR.sd <-  sd(Total.IBNR)
    Total.IBNR.q <- quantile(Total.IBNR, probs=probs,...)

    Totals <-  c(Total.Latest, Total.Latest+Total.IBNR.mean,
                 Total.IBNR.mean, Total.IBNR.sd, Total.IBNR.q)
    Totals <- as.data.frame(Totals)

    colnames(Totals)=c("Totals")
    rownames(Totals) <- c("Latest:","Mean Ultimate:",
                          "Mean IBNR:","SD IBNR:",
                          paste("Total IBNR ", probs*100, "%:", sep="") )

    output <- list(ByOrigin=ByOrigin, Totals=Totals)
    return(output)
}
############################################################################
## print.BootChainLadder
##
print.BootChainLadder <- function(x,probs=c(0.75,0.95),...){
    print(x$call)
    cat("\n")
    summary.x <- summary(x,probs=probs)

    print(format(summary.x$ByOrigin, big.mark = ",", digits = 3),...)

    cat("\n")
    Totals <- summary.x$Totals
    print(format(Totals, big.mark=",",digits=3), quote=FALSE)

  }


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


############################################################################
## plot.BootChainLadder
##
plot.BootChainLadder <- function(x,mfrow=c(2,2),title=NULL,log=FALSE,...){

    if(is.null(title)) myoma <- c(0,0,0,0) else myoma <- c(0,0,2,0)

    Total.IBNR <- x$IBNR.Total

    op=par(mfrow=mfrow, oma=myoma,...)
    ## Histogram
    hist(Total.IBNR, xlab="Total IBNR")
    lines(density(Total.IBNR))
    rug(Total.IBNR)
    ## Empirical distribution
    plot(ecdf(Total.IBNR), xlab="Total IBNR")

    ## Plot simultated Ultiamtes by Origin
    plotBootstrapUltimates(x)

    ## Backtest last developemt year and check for cal. year trends
    backTest <- backTestLatestIncremental(x)
    plotLatestIncremental(backTest, log=log)

    title( title , outer=TRUE)
    par(op)
}



backTestLatestIncremental <- function(x){
    if(!any(class(x) %in% "BootChainLadder"))
       stop("This function expects an object of class BootChainLadder")
    # Simulated Latest Incremental
    simLatest <- getLatest(getIncremental(x$simClaims))

    ## Actual Latest Incremental
    triangle <- x$Triangle
    dim(triangle) <- c(dim(triangle)[1:2],1)
    actual.incr.Latest <- as.vector(getLatest(getIncremental(getIncremental(triangle))))
    df.actual <- data.frame(actual.incr.Latest, origin=dimnames(x$Triangle)[[1]])

    Long.simLatest <- expand.grid(origin=dimnames(x$Triangle)[[1]], sample=c(1:x$R))
    Long.simLatest$sim.incr.Latest <- as.vector(simLatest)
    LongSimActual <- merge(Long.simLatest, df.actual)
    return(LongSimActual)
}

plotLatestIncremental <- function(LongSimActual,##sim.Latest,
                                  ##actual.Latest,
                                  log=TRUE,
                                  xlab="origin period",
                                  ylab="latest incremental claims",
                                  main="Latest actual incremental claims\nagainst simulated values",
                                  col="bisque",
                                  ...){
    ## Plot the latest simulated incremental claims against actual observations to identify calendar year trends
    ## Example:
    ## B <- BootChainLadder(RAA)
    ## x <- backTestLatestIncremental(B)
    ## plotLatestIncremental(x[[1]], x[[1]])

    LongSimActual <- LongSimActual[LongSimActual$sim.incr.Latest>0 & LongSimActual$actual.incr.Latest>0,]
    LongSimActual$origin <- factor(LongSimActual$origin)
    if(log){
        boxplot(LongSimActual$sim.incr.Latest ~ LongSimActual$origin, log="y",
                xlab=xlab, ylab=paste("Log(",ylab,")", sep=""), main=main,col=col,...)
    }else{
        boxplot(LongSimActual$sim.incr.Latest ~ LongSimActual$origin,
                xlab=xlab, ylab=ylab, main=main,col=col,...)
    }
    points(y=LongSimActual$actual.incr.Latest,x=LongSimActual$origin, col=2, pch=19)
    legend("topleft","Latest actual", pch=19, col=2)
}

plotBootstrapUltimates <- function(x, xlab="origin period", ylab="ultimate claims costs",
                                   main="Simulated ultimate claims cost",...){

    triangle <- x$Triangle
    dim(triangle) <- c(dim(triangle),1)
    Latest <- as.vector(getLatest(getIncremental(triangle)))

    .origin <- dimnames(x$Triangle)[[1]]
    meanUlt <- data.frame(mean(x)$ByOrigin+Latest, .origin)
    names(meanUlt) <- c("meanUltimate", "origin")
    Ultimate <- apply(x$IBNR.ByOrigin, 3, function(.x) .x+Latest)


    simUlt <- expand.grid(origin=.origin, sample=c(1:dim(x$IBNR.ByOrigin)[3]))
    simUlt$Ultimate <- as.vector(Ultimate)

    simUlt <- subset(merge(simUlt, meanUlt) , Ultimate > 0)
    simUlt$origin <- factor(simUlt$origin)


    boxplot(Ultimate ~ origin, data=simUlt,
            xlab=xlab, ylab=ylab,main=main,col="bisque",...)

    points(y=simUlt$meanUltimate, x=simUlt$origin,col=2, pch=19)
    legend("topleft","Mean ultimate claim", pch=19, col=2)

}
