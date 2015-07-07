## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:19/10/2008

BootChainLadder <- function(Triangle, R = 999, process.distr=c("gamma", "od.pois")){
  
  
  if(!'matrix' %in% class(Triangle))
    Triangle <- as.matrix(Triangle)
  
  process.distr <- match.arg(process.distr)
  weights <- 1/Triangle
  
  triangle <- Triangle
  if(nrow(triangle) != ncol(triangle))
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
  Latest <- getDiagonal(triangle,m)
  
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
  nobs  <- 0.5 * n * (n + 1)
  scale.factor <- (nobs - 2*n+1)
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
  simCum <- makeCumulative(simClaims)
  simLatest <- getDiagonal(simCum,nrow(simCum))
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
  processTriangle <-array(NA,c(n,n,R))
  
  if(process.distr=="gamma")
    processTriangle[!is.na(simExp)] <- sign(simExp[!is.na(simExp)])*rgamma(length(simExp[!is.na(simExp)]), shape=abs(simExp[!is.na(simExp)]/scale.phi), scale=scale.phi)
  
  if(process.distr=="od.pois")
    processTriangle <-  apply(simExp,c(1,2,3), function(x)
      ifelse(is.na(x), NA, sign(x)*rpois.od(1, abs(x), scale.phi)))
  ##if(process.distr=="od.nbionm")
  ##  processTriangle <-  apply(simExp,c(1,2,3), function(x)
  ##          ifelse(is.na(x), NA, sign(x)*rnbinom.od(1, mu=abs(x), size=sqrt(1+abs(x)),d=scale.phi)))
  
  
  processTriangle[is.na(processTriangle)] <- 0
  simExp[is.na(simExp)] <- 0
  
  IBNR.Triangles <- processTriangle
  IBNR <- makeCumulative(IBNR.Triangles)[,n,]
  dim(IBNR)<-c(n,1,R)
  ParamDist <- makeCumulative(simExp)[,n,]
  dim(ParamDist)<-c(n,1,R)
  
  # Giuseppe Crupi
  # Generate and process Next Year triangle for one year re-reserving approach (Solvency 2 purpose)
  
  NYCost<-getNYCost(triangle[origins,,,drop=F],IBNR.Triangles,R)
  NYParamDist<-getNYCost(triangle[origins,,,drop=F],simExp,R)
  
  if(m>n){
    IBNR <- apply(IBNR, 3, function(x) c(rep(0, m-n),x))
    dim(IBNR) <- c(m,1,R)
    
    NYCost <- apply(NYCost, 3, function(x) c(rep(0, m-n),x))
    dim(NYCost) <- c(m,1,R)
    
    ParamDist <- apply(ParamDist, 3, function(x) c(rep(0, m-n),x))
    dim(ParamDist) <- c(m,1,R)
    
    NYParamDist <- apply(NYParamDist, 3, function(x) c(rep(0, m-n),x))
    dim(NYParamDist) <- c(m,1,R)
    
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
                  ParamDist.ByOrigin=ParamDist,
                  NYCost.ByOrigin = NYCost,
                  NYParamDist.ByOrigin=NYParamDist,
                  #NYIBNR.ByOrigin = NYIBNR,
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
  
  names(summary.x$ByOrigin)[4] <- "IBNR.S.E"
  print(format(summary.x$ByOrigin, big.mark = ",", digits = 3),...)
  
  cat("\n")
  Totals <- summary.x$Totals
  rownames(Totals)[4] <- "IBNR.S.E"
  print(format(Totals, big.mark=",",digits=3), quote=FALSE)
  
}


makeCumulative <- function(tri){
  ## Author: Nigel de Silva - Giuseppe Crupi
  for (i in 2:dim(tri)[2])
    tri[,i,]<-tri[,i-1,]+tri[,i,]
  return(tri)
}

getIncremental <- function(tri){
  ## Author: Nigel de Silva - Giuseppe Crupi
  n<-dim(tri)[2]
  tri[,2:n,]<-tri[,2:n,]-tri[,1:(n-1),]
  
  return(tri)
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
  ## Author: Nigel de Silva - Giuseppe Crupi
  .include <- dfs
  .include[!is.na(.include)] <- 1
  .include[is.na(wghts)] <- NA
  .include[is.na(.include)] <- 0    
  dfs[is.na(dfs)]<-0
  wghts[is.na(wghts)]<-0
  
  out<-colSums(dfs * wghts * .include)/colSums(wghts * .include)
  out[is.nan(out)] <- 1
  
  out <- array(out, c(1,dim(out)))
  return(out)
  
}

getUltDFs <- function(avDFs){
  ## Author: Nigel de Silva - Giuseppe Crupi
  for (i in seq.int(dim(avDFs)[2]-1,1,-1))
    avDFs[,i,]<-avDFs[,i+1,]*avDFs[,i,]
  return(avDFs)
  
}

getUltimates <- function(latest, ultDFs){
  ## Author: Nigel de Silva - Giuseppe Crupi
  m<-dim(ultDFs)[1]
  n<-dim(ultDFs)[2]
  r<-dim(ultDFs)[3]    
  
  ultDFs <- ultDFs[,(dim(ultDFs)[2]):1,]
  dim(ultDFs)<-c(n,1,r)
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
# Fri Jun 14 19:31:24 CEST 2002 https://stat.ethz.ch/pipermail/r-help/2002-June/022425.html
# functions to generate random numbers following
# over-dispersed Poisson and negative binomial distributions
# based on simple cheat of using a standard negative binomial,
# but choosing the scale parameter to give the desired mean/variance
# ratio at the given value of the mean.

rpois.od<-function (n, lambda,d=1) {
  if (d==1 | lambda<=0)
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
  rnbinom(n, size2, prob2)
}


############################################################################
## plot.BootChainLadder
##
plot.BootChainLadder <- function(
  x, mfrow=NULL, title=NULL, log=FALSE,
  which=1:4, ...){
    
    if(is.null(title)){ 
      myoma <- c(0,0,0,0) 
    }else{ 
      myoma <- c(0,0,2,0)
    }
    
    Total.IBNR <- x$IBNR.Total
    
    if(is.null(mfrow)){
      mfrow <- c(ifelse(length(which) < 2,1, 
                        ifelse(length(which) < 3, 2,
                               ceiling(length(which)/2))), 
                 ifelse(length(which)>2,2,1))
    }
    
    op=par(mfrow=mfrow, oma=myoma,...)
    
    if(1 %in% which){
      ## Histogram
      hist(Total.IBNR, xlab="Total IBNR")
      lines(density(Total.IBNR))
      rug(Total.IBNR)
    }
    if(2 %in% which){
      ## Empirical distribution
      plot(ecdf(Total.IBNR), xlab="Total IBNR")
    }
    if(3 %in% which){
      ## Plot simultated Ultiamtes by Origin
      plotBootstrapUltimates(x)
    }
    if(4 %in% which){
      ## Backtest last developemt year and check for cal. year trends
      backTest <- backTestLatestIncremental(x)
      plotLatestIncremental(backTest, log=log)
    }
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

getTriangleNextYear <- function(t.orig,t.ibnr){
  ## Author: Giuseppe Crupi
  
  m<-dim(t.orig)[1]
  n<-dim(t.orig)[2]
  r<-dim(t.ibnr)[3]
  
  out<-array(NA,c(m,n,r))  
  out[,,]<-getIncremental(t.orig)[,,]
  IBNRIndexes<-getDiagonalIndexes(t.ibnr,m+1)
  out[IBNRIndexes]<-t.ibnr[IBNRIndexes]
  out<-makeCumulative(out)
  
  return(out)
  
}

getDiagonalIndexes <-function(tri,d){
  ## Author: Giuseppe Crupi
  
  n<-dim(tri)[1]
  m<-dim(tri)[2]
  r<-dim(tri)[3]
  if (is.na(r)) r<-1
  
  endRow<-min(d,n)
  startRow<-max(1,d-m+1)
  
  startCol<-min(d,m)
  endCol<-max(1,d-n+1)
  
  i<-startRow:endRow
  j<-startCol:endCol
  out<-array(0,c(length(i)*r,3))
  
  out[,1]<-rep(i,r)
  out[,2]<-rep(j,r)
  out[,3]<-rep(1:r,each=length(i))
  
  return(out)
  
}


getDiagonal <-function(tri,d){
  ## Author: Giuseppe Crupi
  
  n<-dim(tri)[1]
  m<-dim(tri)[2]
  r<-dim(tri)[3]
  if (is.na(r)) r<-1
  dim(tri)<-c(n,m,r)
  
  i<-getDiagonalIndexes(tri,d)
  out<-tri[i]
  dim(out)<-c(dim(i)[1]/r,1,r)
  
  return(out)
  
}

getNYCost <-function(t.orig,t.ibnr,R){
  
  m <- dim(t.orig)[1]
  n <- dim(t.orig)[2]
  
  triangleNY<-getTriangleNextYear(t.orig,t.ibnr)
  
  NYLatest <- getDiagonal(triangleNY,m+1) ## Cumulative 
  NYPayments <- getDiagonal(t.ibnr,m+1) # Incremental
  
  NYDFs <- getIndivDFs(triangleNY)
  
  NYWghts <- triangleNY
  
  NYAvDFs <- getAvDFs(NYDFs, NYWghts)
  
  NYUltDFs <- getUltDFs(NYAvDFs)[,2:n,]
  dim(NYUltDFs)<-c(1,n-1,R)
  
  NYUlts <- getUltimates(NYLatest, NYUltDFs)
  
  NYIBNR <- NYUlts - NYLatest
  
  out <- array(0,c(m,1,R))
  
  out[2:m,,] <- NYIBNR+NYPayments ## New rereserved ultimate
  
  return(out)
}


CDR.BootChainLadder <- function(x, probs=c(0.75, 0.95), ...){
  B <- x
  if(!"BootChainLadder" %in% class(B))
    stop("The input to CDR.BootChainLadder has to be output of BootChainLadder.")
  
  IBNR <-  c(summary(B)$ByOrigin[,3], summary(B)$Totals[3,1])
  IBNR.S.E <-  c(summary(B)$ByOrigin[,4], summary(B)$Totals[4,1])
  CDR <- apply(B[["NYCost.ByOrigin"]],1, mean)
  CDR.SD <- apply(B[["NYCost.ByOrigin"]],1, sd)
  CDR.Totals <- apply(B[["NYCost.ByOrigin"]],3,sum)
  
  CDR.Param.SD<-apply(B[["NYParamDist.ByOrigin"]],1, sd)
  CDR.Param.Totals <-apply(B[["NYParamDist.ByOrigin"]],3,sum)
  
  #CDR <- IBNR - c(CDR, mean(CDR.Totals))
  CDR.Param.S.E <- c(CDR.Param.SD, sd(CDR.Param.Totals ))
  CDR.S.E <- c(CDR.SD, sd(CDR.Totals ))
  CDR.Process.S.E<-(CDR.S.E^2-CDR.Param.S.E^2)^0.5
  
  if(length(probs)>1){
    CDR.Q <-  t(cbind(apply(B[["NYCost.ByOrigin"]],1, quantile, probs),
                      quantile(CDR.Totals, probs))) #- IBNR
  }else{
    CDR.Q <-  c(apply(B[["NYCost.ByOrigin"]],1,quantile, probs),
                quantile(CDR.Totals, probs)) #- IBNR
  }
  
  res <- data.frame(IBNR, IBNR.S.E, 
                    #CDR.Param.S.E, 
                    #CDR.Process.S.E, 
                    CDR.S.E, CDR.Q)
  
  names(res)[-c(1:2)] <- c("CDR(1)S.E",
                           paste0("CDR(1)", 100*probs,"%"))
  
  rownames(res) <- c(rownames(B$Triangle), "Total")
  return(res)
}
