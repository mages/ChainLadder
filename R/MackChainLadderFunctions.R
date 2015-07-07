
## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:10/11/2007
## Date:08/09/2008
## Date:22/03/2009
## Date:25/05/2010 Dan

MackChainLadder <- function(
  Triangle,
  weights=1,
  alpha=1,
  est.sigma="log-linear",
  tail=FALSE,
  tail.se=NULL,
  tail.sigma=NULL,
  mse.method = "Mack")
{
    ## idea: have a list for tail factor
    ## tail=list(f=FALSE, f.se=NULL, sigma=NULL, F.se=NULL)
    ##
    # 2013-02-25 Parameter risk recursive formula may have a third term per
    #   Murphy and BBMW
    if (! mse.method %in% c("Mack", "Independence")) stop("mse.method must be 'Mack' or 'Independence'")
    
    Triangle <- checkTriangle(Triangle)
    m <- dim(Triangle)[1]
    n <- dim(Triangle)[2]
    
    ## Create chain ladder models
    
    ## Mack uses alpha between 0 and 2 to distinguish
    ## alpha = 0 ordinary regression with intercept 0
    ## alpha = 1 historical chain ladder age-to-age factors
    ## alpha = 2 straight averages
    
    ## However, in Zehnwirth & Barnett they use the notation of delta, whereby delta = 2 - alpha
    ## the delta is than used in a linear modelling context.
    delta <- 2-alpha
    CL <- chainladder(Triangle, weights=weights, delta=delta)
    alpha <- 2 - CL$delta
    
    # Estimate expected values and standard errors in four steps:
    # 1) Squaring the Triangle: Expected values and f/F SE's from the data in the triangle
    # 2) Expected values and f/F SE's from the tail factor specifications
    # 3) Process Risk and Parameter Risk estimates of the squared triangle (incl tail column)
    # 3) Expected values and SE's for the totals-across-origin-periods of the predicted values
    
    
    ## 1) Squaring the Triangle
    
    ## EXPECTED VALUES: Predict the chain ladder models
    FullTriangle <- predict.ChainLadder(list(Models=CL[["Models"]], Triangle=Triangle))
    ## f/F SE's
    StdErr <- Mack.S.E(CL[["Models"]], FullTriangle, est.sigma = est.sigma,
                       weights = CL[["weights"]], alpha = alpha)
    
    ## 2) Tail
    ## Check for tail factor
    if(is.logical(tail)){
      if(tail){
        tail <- tailfactor(StdErr$f)
        tail.factor <- tail$tail.factor
        StdErr$f <- c(StdErr$f, tail.factor = tail.factor)
      }else{
        #            tail.factor <- tail
        # MunichChainLadder needs an 'f' vector as long as the triangle is wide
        #   and adding on a harmless tail doesn't seem to cause any 
        #   Mack difficulties
        # Default will not be named in the output
        tail.factor <- 1.000
        StdErr$f <- c(StdErr$f, tail.factor)
      }
    }else{
      #        if(is.numeric(tail))
      # Documentation says tail must be logic or numeric
      tail.factor <- as.numeric(tail)
      StdErr$f <- c(StdErr$f, tail.factor = tail.factor)
    }
    # Then finally, ...
    if (tail.factor > 1) {
      ## EXPECTED VALUES
      FullTriangle <- tail.E(FullTriangle, tail.factor)
      ## STANDARD ERRORS
      ## Estimate the standard error of f and F in the tail
      ##  If tail.se and/or tail.sigma provided, return those values
      StdErr <- tail.SE(FullTriangle, StdErr, Total.SE, tail.factor,
                        tail.se = tail.se, tail.sigma = tail.sigma)
    }
    
    ## 3) Calculate process and parameter risks of the predicted loss amounts
    StdErr <- c(StdErr, MackRecursive.S.E(FullTriangle, StdErr$f, StdErr$f.se, StdErr$F.se, mse.method = mse.method))
    
    ## 4) Total-across-origin-periods by development period
    ## EXPECTED VALUES
    ##   Not complicated. Not required at this time.
    ## STANDARD ERRORS
    ## Calculate process and parameter risk for the sum of the predicted loss amounts
    Total.SE <- TotalMack.S.E(FullTriangle, StdErr$f, StdErr$f.se, StdErr$F.se, StdErr$FullTriangle.procrisk, mse.method = mse.method)
    
    ## Collect the output
    output <- list()
    output[["call"]] <-  match.call(expand.dots = FALSE)
    output[["Triangle"]] <- Triangle
    output[["FullTriangle"]] <- FullTriangle
    output[["Models"]] <- CL[["Models"]]
    output[["f"]] <- StdErr$f
    output[["f.se"]] <- StdErr$f.se
    output[["F.se"]] <- StdErr$F.se
    output[["sigma"]] <- StdErr$sigma
    output[["Mack.ProcessRisk"]]   <- StdErr$FullTriangle.procrisk  # new dmm
    output[["Mack.ParameterRisk"]] <- StdErr$FullTriangle.paramrisk  # new dmm
    output[["Mack.S.E"]] <- sqrt(StdErr$FullTriangle.procrisk^2 + StdErr$FullTriangle.paramrisk^2)
    output[["weights"]] <- CL$weights
    output[["alpha"]] <- alpha
    ## total.procrisk <- apply(StdErr$FullTriangle.procrisk, 2, function(x) sqrt(sum(x^2)))
    output[["Total.Mack.S.E"]] <- Total.SE[1] # [1] removes attributes
    output[["Total.ProcessRisk"]] <- attr(Total.SE, "processrisk")
    output[["Total.ParameterRisk"]] <- attr(Total.SE, "paramrisk")
    output[["tail"]] <- tail
    class(output) <- c("MackChainLadder", "TriangleModel", "list")
    return(output)
  }

##############################################################################
## Calculation of the mean squared error and standard error
## mean squared error = stochastic error (process variance) + estimation error
## standard error = sqrt(mean squared error)

Mack.S.E <- function(MackModel, FullTriangle, est.sigma="log-linear", weights, alpha) {
  n <- ncol(FullTriangle)
  m <- nrow(FullTriangle)
  f <- rep(1, n - 1)
  f.se <- rep(0, n - 1)
  sigma <- rep(0, n - 1)
  
  ## Extract estimated slopes, std. error and sigmas
  f <- sapply(MackModel, function(x) summary(x)$coef["x","Estimate"])
  f.se <- sapply(MackModel, function(x) summary(x)$coef["x","Std. Error"])
  sigma <- sapply(MackModel, function(x) summary(x)$sigma)
  
  isna <- is.na(sigma)
  ## Think about weights!!!
  
  if(est.sigma[1] %in% "log-linear"){
    if (sum(!isna) == 1) {
      warning(paste("Too few (1) link ratios for fitting 'loglinear' model to estimate sigma_n.",
                    "est.sigma will be overwritten to 'Mack'.\n",
                    "Mack's estimation method will be used instead."))
      est.sigma <- "Mack"
    }
    else {
      ## estimate sigma[n-1] via log-linear regression
      sig.model <- estimate.sigma(sigma)
      sigma <- sig.model$sigma
      
      p.value.of.model <- summary(sig.model$model)$coefficient[2,4]
      if(p.value.of.model > 0.05){
        warning(paste("'loglinear' model to estimate sigma_n doesn't appear appropriate.",
                      "\np-value > 5.\n",
                      "est.sigma will be overwritten to 'Mack'.\n",
                      "Mack's estimation method will be used instead."))
        
        est.sigma <- "Mack"
      }else{
        f.se[isna] <- sigma[isna]/sqrt(weights[1,isna]*FullTriangle[1,isna]^alpha[isna])
      }
    }
  }
  if(est.sigma[1] %in% "Mack"){
    for(i in which(isna)){   # usually i = n - 1
      sigma[i] <- sqrt(abs(min((sigma[i - 1]^4/sigma[i - 2]^2),
                               min(sigma[i - 2]^2, sigma[i - 1]^2))))
      f.se[i] <- sigma[i]/sqrt(weights[1,i]*FullTriangle[1,i]^alpha[i])
    }
  }
  if(is.numeric(est.sigma)){
    # Markus, I'm not sure what your intention is in this next loop when the lengths of est.sigma and sigma differ
    for(i in seq(along=est.sigma)){
      l <- length(est.sigma)
      # BUG?! If length(est.sigma) > n, then n-i could be negative
      sigma[n-i] <- est.sigma[l-i+1]
      f.se[n-i] <- sigma[n-i]/sqrt(weights[1,n-i]*FullTriangle[1,n-i]^alpha[n-i])
    }
  }
  
  W <- weights
  W[is.na(W)] <- 1
  F.se <- t(sigma/t(sqrt(W[,-n]*t(t(FullTriangle[,-n])^alpha[-n]))))
  
  return(list(sigma = sigma,
              f = f,
              f.se = f.se,
              F.se = F.se)
  )
}
################################################################
MackRecursive.S.E <- function(FullTriangle, f, f.se, F.se, mse.method = "Mack"){
  n <- ncol(FullTriangle)
  m <- nrow(FullTriangle)
  
  FullTriangle.procrisk <- FullTriangle[, 1:n] * 0
  FullTriangle.paramrisk <- FullTriangle[, 1:n] * 0
  
  ## Recursive Formula
  colindex <- 1:(n-1)
  for (k in colindex) {
    for (i in (m-k+1):m) {
      FullTriangle.procrisk[i,k+1] <- sqrt(
        FullTriangle[i,k]^2*(F.se[i,k]^2)
        + FullTriangle.procrisk[i,k]^2*f[k]^2
      )
      FullTriangle.paramrisk[i,k+1] <- sqrt(
        FullTriangle[i,k]^2*(f.se[k]^2)
        + FullTriangle.paramrisk[i,k]^2*f[k]^2
        # 2013-02-25 Parameter risk recursive formula may have a third term
        + ifelse(mse.method == "Mack", 0, FullTriangle.paramrisk[i, k]^2 * (f.se[k]^2))
      )
    }
  }
  
  return(list(FullTriangle.procrisk=FullTriangle.procrisk,
              FullTriangle.paramrisk=FullTriangle.paramrisk))
}

################################################################################
## Total reserve SE

TotalMack.S.E <- function(FullTriangle, f, f.se, F.se, FullTriangle.procrisk, mse.method = "Mack") {
  # 2013-02-25
  # Parameter Risk and Process Risk components of Total Risk broken out
  
  # The current program design expects a scalar, total.seR, from this 
  #   function equal to the total risk of the total reserve.
  #   So as not to break existing code, the Parameter Risk and 
  #   Process Risk vectors (one element per column) are attached as attributes 
  
  C <- FullTriangle
  n <- ncol(C)
  m <- nrow(C)
  
  total.paramrisk <- numeric(n)
  
  # Assumption is that origin years are independent, therefore
  #    process variance is additive
  total.procrisk <- sqrt(colSums(FullTriangle.procrisk^2, na.rm = TRUE))
  
  # For parameter risk recursion, the sum of future loss expected values
  #   plus the diagonal value, by development age, comes in handy.
  # Currently, code relies on the fact that nrows(triangle) >= ncols
  #   Actually, function 'checkTriangle' makes sure Triangle is square
  M <- sapply(1:ncol(FullTriangle), function(k) sum(FullTriangle[(m + 1 - k):m, k], na.rm = TRUE))
  
  for(k in c(1:(n-1))){
    #        total.seR[k+1] <- sqrt(total.seR[k]^2 * f[k]^2 +
    #                               sum(C[c((m+1-k):m),k]^2 *
    #                                   (F.se[c((m+1-k):n),k]^2),na.rm=TRUE)
    #                               + sum(C[c((m+1-k):m),k],na.rm=TRUE)^2 * f.se[k]^2 )
    total.paramrisk[k + 1] <- sqrt(
      sum(M[k], na.rm = TRUE)^2 * f.se[k]^2 +
        total.paramrisk[k]^2 * f[k]^2 +
        ifelse(mse.method == "Mack", 0, total.paramrisk[k]^2 * f.se[k]^2))
  }
  
  # The scalar returned is the total risk of total reserves in the rightmost column
  total.seR <- sqrt(total.procrisk^2 + total.paramrisk^2)[n]
  attr(total.seR, "processrisk") <- total.procrisk
  attr(total.seR, "paramrisk") <- total.paramrisk
  
  return(total.seR)
}

##############################################################################

estimate.sigma <- function(sigma){
  if(!all(is.na(sigma))){
    n <- length(sigma)
    dev <- 1:n
    my.dev <- dev[!is.na(sigma) & sigma > 0]
    my.model <- lm(log(sigma[my.dev]) ~ my.dev)
    sigma[is.na(sigma)] <- exp(predict(my.model, newdata=data.frame(my.dev=dev[is.na(sigma)])))
  }
  
  return(list(sigma=sigma, model=my.model))
}


########################################################################
## Estimate expected value when tail

tail.E <- function(FullTriangle, tail.factor){
  n <- ncol(FullTriangle)
  m <- nrow(FullTriangle)
  
  FullTriangle <- cbind(FullTriangle, FullTriangle[,n] * tail.factor)
  dimnames(FullTriangle) <- list(origin=dimnames(FullTriangle)[[1]],
                                 dev=c(dimnames(FullTriangle)[[2]][1:n], "Inf"))
  return(FullTriangle)
}

########################################################################
## Estimate standard error for tail

tail.SE <- function(FullTriangle, StdErr, Total.SE, tail.factor, tail.se = NULL, tail.sigma = NULL) {
  n <- ncol(FullTriangle)
  m <- nrow(FullTriangle)
  
  ## Idea: linear model for f, estimate dev for tail factor
  ## linear model for f.se and sigma and put dev from above in
  ## 2013-02-28: The tail factor is given and stored in StdEff$f[n-1];
  ##    want to relate the f.se's and sigma's from link ratios estimated
  ##    from the interior of the triangle to the given tail. 
  ##    The link ratios from the interior of the triangle are stored in
  ##    StdEff$f[1:(n-2)].
  stopifnot(n > 2) # Must have at least two columns in the original triangle
  # to impute estimates for the tail-driven, 
  # previously c-bind-ed Ultimate column
  start <- 1
  .f <- StdErr$f[start:(n - 2)]
  .dev <- c(start:(n - 2))
  ##    mf <- lm(log(.f[.f>1]-1) ~ .dev[.f>1])
  mf <- lm(log(.f-1) ~ .dev)
  tail.pos <- (log(StdErr$f[n - 1] - 1) - coef(mf)[1]) / coef(mf)[2]
  
  if(is.null(tail.se)){
    .fse <- StdErr$f.se[start:(n-2)]
    mse <- lm(log(.fse) ~ .dev)
    tail.se <- exp(predict(mse, newdata=data.frame(.dev=tail.pos)))
  }
  StdErr$f.se <- c(StdErr$f.se, tail.se = tail.se)
  
  if(is.null(tail.sigma)){
    .sigma <- StdErr$sigma[start:(n-2)]
    msig <- lm(log(.sigma) ~ .dev)
    tail.sigma <- exp(predict(msig, newdata = data.frame(.dev = tail.pos)))
  }
  StdErr$sigma <- c(StdErr$sigma, tail.sigma = as.numeric(tail.sigma))
  
  ## estimate the standard error of the tail factor ratios
  se.F.tail <- tail.sigma / sqrt(FullTriangle[, n - 1])
  StdErr$F.se <- cbind(StdErr$F.se, se.F.tail)
  
  return(StdErr)
}




##############################################################################
## Summary
##
summary.MackChainLadder <- function(object,...){
  ## Summarise my results
  Latest <- getLatestCumulative(object$Triangle)
  
  ex.origin.period <- Latest!=0
  
  Ultimate <- object[["FullTriangle"]][,ncol(object[["FullTriangle"]])]
  Dev.To.Date <- Latest/Ultimate
  IBNR <- Ultimate-Latest
  Mack.S.E <- object[["Mack.S.E"]][,ncol(object[["Mack.S.E"]])]
  CV <- Mack.S.E/(Ultimate-Latest)
  
  ByOrigin <- data.frame(Latest, Dev.To.Date, Ultimate, IBNR, Mack.S.E, CV)
  names(ByOrigin)[6]="CV(IBNR)"
  ByOrigin <- ByOrigin[ex.origin.period,]
  
  Totals <-  c(sum(Latest,na.rm=TRUE),
               sum(Latest,na.rm=TRUE)/sum(Ultimate,na.rm=TRUE),
               sum(Ultimate,na.rm=TRUE),
               sum(IBNR,na.rm=TRUE), object[["Total.Mack.S.E"]],
               object[["Total.Mack.S.E"]]/sum(IBNR,na.rm=TRUE)
  )
  # Totals <- c(Totals, round(x[["Total.Mack.S.E"]]/sum(res$IBNR,na.rm=TRUE),2))
  Totals <- as.data.frame(Totals)
  
  colnames(Totals)=c("Totals")
  rownames(Totals) <- c("Latest:","Dev:","Ultimate:",
                        "IBNR:","Mack S.E.:",
                        "CV(IBNR):")
  
  output <- list(ByOrigin=ByOrigin, Totals=Totals)
  return(output)
}

#getLatestCumulative <- function(cumulative.tri)
#  apply(cumulative.tri, 1L, function(x) ifelse(length(w <- which(!is.na(x))) > 0L, x[tail(w, 1L)], x[1L]))

##############################################################################
## print
##
print.MackChainLadder <- function(x,...){
  
  summary.x <- summary(x)
  print(x$call)
  cat("\n")
  print(format(summary.x$ByOrigin, big.mark = ",", digits = 3),...)
  
  Totals <- summary.x$Totals
  rownames(Totals)[5] <- "Mack.S.E"
  Totals[1:6,] <- formatC(Totals[1:6,], big.mark=",",digits=2,format="f")
  cat("\n")
  print(Totals, quote=FALSE)
  #invisible(x)
  
}


################################################################################
## plot
##
plot.MackChainLadder <- function(
  x, mfrow=NULL, title=NULL,
  lattice=FALSE, which=1:6, ...){
    
    .myResult <-  summary(x)$ByOrigin
    
    .FullTriangle <- x[["FullTriangle"]]
    .Triangle <- x[["Triangle"]]
    
    if(is.null(mfrow)){
      mfrow <- c(ifelse(length(which) < 2,1, 
                        ifelse(length(which) < 3, 2,
                               ceiling(length(which)/2))), 
                 ifelse(length(which)>2,2,1))
    }
    if(!lattice){
      if(is.null(title)) myoma <- c(0,0,0,0) else myoma <- c(0,0,2,0)
      
      op=par(mfrow=mfrow, oma=myoma, mar=c(4.5,4.5,2,2))
      
      plotdata <- t(as.matrix(.myResult[,c("Latest","IBNR")]))
      n <- ncol(plotdata)
      
      if(1 %in% which){
        
        if(getRversion() < "2.9.0") { ## work around missing feature
          
          bp <- barplot(plotdata,
                        legend.text=c("Latest","Forecast"),
                        ##    args.legend=list(x="topleft"), only avilable from R version >= 2.9.0
                        names.arg=rownames(.myResult),
                        main="Mack Chain Ladder Results",
                        xlab="Origin period",
                        ylab="Amount",#paste(Currency,myUnit),
                        ylim=c(0, max(apply(.myResult[c("Ultimate", "Mack.S.E")],1,sum),na.rm=TRUE)))
          
        }else{
          bp <- barplot(plotdata,
                        legend.text=c("Latest","Forecast"),
                        args.legend=list(x="topleft"),
                        names.arg=rownames(.myResult),
                        main="Mack Chain Ladder Results",
                        xlab="Origin period",
                        ylab="Amount",#paste(Currency,myUnit),
                        ylim=c(0, max(apply(.myResult[c("Ultimate", "Mack.S.E")],1,sum),na.rm=TRUE)))
        }
        ## add error ticks
        ## require("Hmisc")
        .errbar(x=bp, y=.myResult$Ultimate,
               yplus=(.myResult$Ultimate + .myResult$Mack.S.E),
               yminus=(.myResult$Ultimate - .myResult$Mack.S.E),
               cap=0.05,
               add=TRUE)
      }
      if(2 %in% which){
        
        matplot(t(.FullTriangle), type="l",
                main="Chain ladder developments by origin period",
                xlab="Development period", ylab="Amount", #paste(Currency, myUnit)
        )
        matplot(t(.Triangle), add=TRUE)
      }
      Residuals=residuals(x)
      if(3 %in% which){
        plot(standard.residuals ~ fitted.value, data=Residuals,
             ylab="Standardised residuals", xlab="Fitted")
        lines(lowess(Residuals$fitted.value, Residuals$standard.residuals), col="red")
        abline(h=0, col="grey")
      }
      if(4 %in% which){
        plot(standard.residuals ~ origin.period, data=Residuals,
             ylab="Standardised residuals", xlab="Origin period")
        lines(lowess(Residuals$origin.period, Residuals$standard.residuals), col="red")
        abline(h=0, col="grey")
      }
      if(5 %in% which){
        plot(standard.residuals ~ cal.period, data=Residuals,
             ylab="Standardised residuals", xlab="Calendar period")
        lines(lowess(Residuals$cal.period, Residuals$standard.residuals), col="red")
        abline(h=0, col="grey")
      }
      if(6 %in% which){
        plot(standard.residuals ~ dev.period, data=Residuals,
             ylab="Standardised residuals", xlab="Development period")
        lines(lowess(Residuals$dev.period, Residuals$standard.residuals), col="red")
        abline(h=0, col="grey")
      }
      title( title , outer=TRUE)
      par(op)
      
    }else{
      
      ## require(grid)
      ## Set legend 
      fl <-
        grid.layout(nrow = 2, ncol = 4,
                    heights = unit(rep(1, 2), "lines"),
                    widths =
                      unit(c(2, 1, 2, 1),
                           c("cm", "strwidth", "cm",
                             "strwidth"),
                           data = list(NULL, "Chain ladder dev.", NULL,
                                       "Mack's S.E.")))
      
      foo <- frameGrob(layout = fl)
      
      foo <- placeGrob(foo,
                       linesGrob(c(0.2, 0.8), c(.5, .5),
                                 gp = gpar(col=1, lty=1)),
                       row = 1, col = 1)
      foo <- placeGrob(foo,
                       linesGrob(c(0.2, 0.8), c(.5, .5),
                                 gp = gpar(col=1, lty=3)), 
                       row = 1, col = 3)
      foo <- placeGrob(foo,
                       textGrob(label = "Chain ladder dev."), 
                       row = 1, col = 2)
      foo <- placeGrob(foo,
                       textGrob(label = "Mack's S.E."), 
                       row = 1, col = 4)
      
      long <- expand.grid(origin=as.numeric(dimnames(.FullTriangle)$origin),
                          dev=as.numeric(dimnames(.FullTriangle)$dev))
      long$value <- as.vector(.FullTriangle)
      long$valuePlusMack.S.E <-  long$value + as.vector(x$Mack.S.E)
      long$valueMinusMack.S.E <- long$value - as.vector(x$Mack.S.E)
      sublong <- long[!is.na(long$value),]
      xyplot(valuePlusMack.S.E + valueMinusMack.S.E + value ~ dev |
               factor(origin), data=sublong, t="l", lty=c(3,3,1), as.table=TRUE,
             main="Chain ladder developments by origin period",
             xlab="Development period",
             ylab="Amount",col=1,
             legend = list(top = list(fun = foo)),...)
    }
  }
################################################################################
## residuals
##
residuals.MackChainLadder <- function(object,...){
  m <- nrow(object[["Triangle"]])
  n <- ncol(object[["Triangle"]])
  myresiduals <- unlist(lapply(object[["Models"]], resid,...))
  
  standard.residuals <- rep(NA, length(myresiduals))
  
  ## Identify if we have standardized residuals for all items, 
  ## e.g. this is not the case if some weights have been set to zero
  x=lapply(lapply(object$Model, resid), function(x) as.numeric(names(x)))
  y=lapply(lapply(object$Model, rstandard), function(x) as.numeric(names(x)))
  names(y)=1:length(y)
  names(x)=1:length(x)		
  rst <- unlist(lapply(names(x), function(z) x[[z]] %in% y[[z]]))
  ## rst holds the indices with standardised residuals for weights > 0 
  
  standard.residuals[rst] <- unlist(lapply(object[["Models"]], rstandard,...))
  fitted.value <- unlist(lapply(object[["Models"]], fitted))
  origin.period <- as.numeric(unlist(lapply(lapply(object[["Models"]],residuals),names)))
  dev.period <-rep(1:(n-1), sapply(lapply(object[["Models"]],residuals),length))
  cal.period <- origin.period + dev.period - 1
  
  myResiduals <- data.frame(origin.period,
                            dev.period,
                            cal.period,
                            residuals=myresiduals,
                            standard.residuals,
                            fitted.value)
  
  return(na.omit(myResiduals))
}


Mack <- function(triangle, weights=1, alpha=1){
  ## Mack Chain Ladder:
  ## triangle: cumulative claims triangle
  ## weights : weights
  ## alpha=0 : gives straight average of the chain ladder age-to-age factors
  ## alpha=1 : gives the historical chain ladder age-to-age factors
  ## alpha=2 : is the result of an ordinary regression of C_{i,k+1} against C_{i,k} with intercept 0.
  weights <- checkWeights(weights, triangle)
  
  m <- nrow(triangle)
  n <- ncol(triangle)
  
  ## Individual chain ladder age-to-age factors:
  F <- triangle[,-1]/triangle[,-n]
  #    if(is.numeric(weights)){
  #        weights2 <- triangle
  #        weights2[!is.na(weights2)] <- weights
  #        weights <- weights2
  #    }
  
  wCa <- weights[,-n] * triangle[,-n]^alpha
  ## Note NA^0=1, hence set all cells which are originally NA back to NA
  wCa[is.na(triangle[,-n])] <- NA
  
  wCaF <- wCa*F
  ## Set diagonal of wCa to NA
  wCa[row(wCa)==n+1-col(wCa)] <- NA
  ## Chain-ladder age-to-age factors
  f <- apply( wCaF, 2, sum, na.rm=TRUE)/apply( wCa, 2, sum, na.rm=TRUE)
  
  ## Get full triangle
  avDFs <- c(f,1)
  dim(avDFs) <- c(1,n,1)
  ultDFs <- getUltDFs(avDFs)
  
  Latest <- getLatestCumulative(triangle)
  ults <- getUltimates(Latest, ultDFs)
  FullTriangle <- getExpected(ults, 1/ultDFs)
  dim(FullTriangle)=c(n,n)
  
  ## Estimate standard errors
  
  k <- c(1:(n-2))
  wCa_F_minus_f <- weights[,k] * triangle[,k]^alpha * t(t(F)-f)[,k]^2
  
  wCa_F_minus_f <-  triangle[,k]^alpha * t(t(F)-f)[,k]^2
  
  sigma <- sqrt( 1/c(n-k-1) * apply(wCa_F_minus_f, 2, sum, na.rm=TRUE) )
  # Mack approximation for sigma_{n-1}
  sigma_n1 <- sqrt( abs(min(sigma[n-2]^4/sigma[n-3]^2, min(sigma[n-3]^2, sigma[n-2]^2)) ))
  sigma <- c(sigma, sigma_n1)
  
  # Estimate f.se
  wCa2 <-  weights[,c(1:(n-1))] * triangle[,c(1:(n-1))]^alpha
  ## Note NA^0=1, hence set all cells which are originally NA back to NA
  wCa2[is.na(triangle[,-n])] <- NA
  
  ## Set diagonal of wCa to na
  wCa2[row(wCa2)==n+1-col(wCa2)] <- NA
  f.se <- sigma / sqrt( apply(wCa2, 2, sum, na.rm=TRUE))
  
  # Estimate F.se
  W <- weights
  W[is.na(W)] <- 1
  F.se <- t(sigma/sqrt(t(W[,c(1:(n-1))]*FullTriangle[,c(1:(n-1))]^alpha)))
  #    F.se <- t(sigma/sqrt(t(FullTriangle[,c(1:(n-1))]^alpha)))
  
  F.se[is.na(FullTriangle[,c(1:(n-1))])] <- NA
  
  FullTriangle.se <- FullTriangle * 0
  ## Recursive Formula
  rowindex <- 2:m
  if(m>n)
    rowindex <- c((m-n+1):m)
  for(i in rowindex){
    for(k in c((n+1-i):(n-1))){
      if(k>0)
        FullTriangle.se[i,k+1] = sqrt(
          FullTriangle[i,k]^2*(F.se[i,k]^2+f.se[k]^2) #
          + FullTriangle.se[i,k]^2*f[k]^2
        )
    }
  }
  
  output <- list()
  output[["call"]] <-  match.call(expand.dots = FALSE)
  output[["Triangle"]] <- triangle
  output[["Latest"]] <- Latest
  output[["FullTriangle"]] <- FullTriangle
  output[["f"]] <- f
  output[["F"]] <- F
  output[["f.se"]] <- f.se
  output[["F.se"]] <- F.se
  output[["sigma"]] <-sigma
  output[["Mack.S.E"]] <- FullTriangle.se
  class(output) <- c("Mack","list")
  return(output)
}

print.Mack <- function(x,...){
  n <- ncol(x$Triangle)
  df <- data.frame(Latest=x$Latest, f=c(NA, x$f), f.se=c(NA, x$f.se),
                   Ultimate=x$FullTriangle[,n])
  df$IBNR <- df$Ultimate-df$Latest
  df$Mack.S.E=x$Mack.S.E[,n]
  df$CV=df$Mack.S.E/df$IBNR
  print(df)
}



.errbar <- function (
  x, y, yplus, yminus, cap = 0.015, 
  main = NULL, sub = NULL, 
  xlab = as.character(substitute(x)), 
  ylab = if (is.factor(x) || is.character(x)) "" else as.character(substitute(y)), 
  add = FALSE, lty = 1, type = "p", ylim = NULL, lwd = 1, pch = 16, 
  errbar.col = par("fg"), Type = rep(1, length(y)), ...) 
{
    # Based on code by Frank Harrell, Hmisc package, licence: GPL >= 2
    if (is.null(ylim)) 
      ylim <- range(y[Type == 1], yplus[Type == 1], yminus[Type == 
                                                             1], na.rm = TRUE)
    if (is.factor(x) || is.character(x)) {
      x <- as.character(x)
      n <- length(x)
      t1 <- Type == 1
      t2 <- Type == 2
      n1 <- sum(t1)
      n2 <- sum(t2)
      omai <- par("mai")
      mai <- omai
      mai[2] <- max(strwidth(x, "inches")) + 0.25
      par(mai = mai)
      on.exit(par(mai = omai))
      plot(NA, NA, xlab = ylab, ylab = "", xlim = ylim, ylim = c(1, 
                                                                 n + 1), axes = FALSE, ...)
      axis(1)
      w <- if (any(t2)) 
        n1 + (1:n2) + 1
      else numeric(0)
      axis(2, at = c(seq.int(length.out = n1), w), labels = c(x[t1], 
                                                              x[t2]), las = 1, adj = 1)
      points(y[t1], seq.int(length.out = n1), pch = pch, type = type, 
             ...)
      segments(yplus[t1], seq.int(length.out = n1), yminus[t1], 
               seq.int(length.out = n1), lwd = lwd, lty = lty, col = errbar.col)
      if (any(Type == 2)) {
        abline(h = n1 + 1, lty = 2, ...)
        offset <- mean(y[t1]) - mean(y[t2])
        if (min(yminus[t2]) < 0 & max(yplus[t2]) > 0) 
          lines(c(0, 0) + offset, c(n1 + 1, par("usr")[4]), 
                lty = 2, ...)
        points(y[t2] + offset, w, pch = pch, type = type, 
               ...)
        segments(yminus[t2] + offset, w, yplus[t2] + offset, 
                 w, lwd = lwd, lty = lty, col = errbar.col)
        at <- pretty(range(y[t2], yplus[t2], yminus[t2]))
        axis(side = 3, at = at + offset, labels = format(round(at, 
                                                               6)))
      }
      return(invisible())
    }
    if (add) 
      points(x, y, pch = pch, type = type, ...)
    else plot(x, y, ylim = ylim, xlab = xlab, ylab = ylab, pch = pch, 
              type = type, ...)
    xcoord <- par()$usr[1:2]
    smidge <- cap * (xcoord[2] - xcoord[1])/2
    segments(x, yminus, x, yplus, lty = lty, lwd = lwd, col = errbar.col)
    if (par()$xlog) {
      xstart <- x * 10^(-smidge)
      xend <- x * 10^(smidge)
    }
    else {
      xstart <- x - smidge
      xend <- x + smidge
    }
    segments(xstart, yminus, xend, yminus, lwd = lwd, lty = lty, 
             col = errbar.col)
    segments(xstart, yplus, xend, yplus, lwd = lwd, lty = lty, 
             col = errbar.col)
    return(invisible())
  }