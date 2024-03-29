
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
  ## alpha = 0 straight averages
  ## alpha = 1 historical chain ladder age-to-age factors
  ## alpha = 2 ordinary regression with intercept 0
  
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
    FullTriangle <- tail_E(FullTriangle, tail.factor)
    ## STANDARD ERRORS
    ## Estimate the standard error of f and F in the tail
    ##  If tail.se and/or tail.sigma provided, return those values
    StdErr <- tail_SE(FullTriangle, StdErr, Total.SE, tail.factor,
                      tail.se = tail.se, tail.sigma = tail.sigma,
                      alpha = alpha)
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
approx.equal <- function (x, y, tol=.Machine$double.eps^0.5) abs(x-y)<tol
Mack.S.E <- function(MackModel, FullTriangle, est.sigma="log-linear", weights, alpha) {
  n <- ncol(FullTriangle)
  m <- nrow(FullTriangle)
  f <- rep(1, n - 1)
  f.se <- rep(0, n - 1)
  sigma <- rep(0, n - 1)
  
  ## Extract estimated slopes, std. error and sigmas
  ## 2015-10-9 Replace lm's warning with more appropriate message
  smmry <- suppressWarnings(lapply(MackModel, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  df <- sapply(smmry, function(x) x$df[2L])
  tolerance <- .Machine$double.eps
  perfect.fit <- (df > 0) & (f.se < tolerance)
  w <- which(perfect.fit)
  if (length(w)) {
    warn <- "Information: essentially no variation in development data for period(s):\n"
    nms <- colnames(FullTriangle)
    periods <- paste0("'", paste(nms[w], nms[w+1], sep = "-"), "'")
    warn <- c(warn, paste(periods, collapse = ", "))
    # Print warning message
    warning(warn)
  }
  #   
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
      sig.model <- suppressWarnings(estimate.sigma(sigma))
      sigma <- sig.model$sigma
      
      p.value.of.model <- tryCatch(summary(sig.model$model)$coefficient[2,4],
                                   error = function(e) e)
      if (inherits(p.value.of.model, "error") |
          is.infinite(p.value.of.model) |
          is.nan(p.value.of.model)
      ) {
        warning(paste("'loglinear' model to estimate sigma_n doesn't appear appropriate.\n",
                      "est.sigma will be overwritten to 'Mack'.\n",
                      "Mack's estimation method will be used instead."))
        est.sigma <- "Mack"
      }
      else
        if(p.value.of.model > 0.05){
          warning(paste("'loglinear' model to estimate sigma_n doesn't appear appropriate.",
                        "\np-value > 5.\n",
                        "est.sigma will be overwritten to 'Mack'.\n",
                        "Mack's estimation method will be used instead."))
          
          est.sigma <- "Mack"
        }
      else{
        f.se[isna] <- sigma[isna]/sqrt(weights[1,isna]*FullTriangle[1,isna]^alpha[isna])
      }
    }
  }
  if(est.sigma[1] %in% "Mack"){
    for(i in which(isna)){   # usually i = n - 1
      ratio <- (sigma[i - 1]^4/sigma[i - 2]^2) # Bug fix: sigma[i - 2]^2 could be zero
      if(is.nan(ratio) | is.infinite(ratio)){ 
        sigma[i] <- sqrt(abs(min(sigma[i - 2]^2, sigma[i - 1]^2)))  
      }else{
        sigma[i] <- sqrt(abs(min(ratio, min(sigma[i - 2]^2, sigma[i - 1]^2))))  
      }
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

tail_E <- function(FullTriangle, tail.factor){
  n <- ncol(FullTriangle)
  m <- nrow(FullTriangle)
  
  FullTriangle <- cbind(FullTriangle, FullTriangle[,n] * tail.factor)
  dimnames(FullTriangle) <- list(origin=dimnames(FullTriangle)[[1]],
                                 dev=c(dimnames(FullTriangle)[[2]][1:n], "Inf"))
  return(FullTriangle)
}

########################################################################
## Estimate standard error for tail

tail_SE <- function(FullTriangle, StdErr, Total.SE, tail.factor, 
                    tail.se = NULL, tail.sigma = NULL, alpha) {
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
  se.F.tail <- tail.sigma / sqrt(FullTriangle[, n - 1]^alpha[n-2])
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

  invisible(x)
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


quantile.MackChainLadder <- function(x, probs=c(0.75, 0.95), na.rm = FALSE,
                                     names = TRUE, type = 7,...){
  
  if(! ("MackChainLadder" %in% class(x))){
    stop("x is not a MackChainLadder output")
  }
  
  Latest <- getLatestCumulative(x$Triangle)
  Ultimate <- x[["FullTriangle"]][,ncol(x[["FullTriangle"]])]
  Dev.To.Date <- Latest/Ultimate
  IBNR <- Ultimate-Latest
  Mack.S.E <- x[["Mack.S.E"]][,ncol(x[["Mack.S.E"]])]
  
  CV <- Mack.S.E/IBNR
  
  Skewn <- Asymetrie(x)
  
  ## Cornish-Fisher
  quantile <- qnorm(probs)
  
  ## Cornish-Fisher: by origin period
  
  CF <- (apply(t(quantile), 2, function(q) 
    q + 1/6 * (q^2 - 1) * Skewn$Skewnes))
  QuantilePY <- (1 + CF * CV) * IBNR
  
  ## Cornish-Fisher: totals across origin periods
  CF <- quantile + 1/6*(quantile^2-1) * Skewn$OverSkew/x[["Total.Mack.S.E"]]^3
  TotQuantile <- (1 + CF*x[["Total.Mack.S.E"]]/sum(IBNR))*sum(IBNR)
  TotSkew <- Skewn$OverSkew/x[["Total.Mack.S.E"]]^3
  
  ByOrigin <- data.frame(Skewness=Skewn$Skewnes, 
                         (QuantilePY))
  names(ByOrigin) <- c("Skewness", paste("IBNR ", probs*100, "%", sep=""))
  
  origin <- dimnames(x$Triangle)[[1]]
  
  if(length(origin)==nrow(ByOrigin)){
    rownames(ByOrigin) <- origin
  }
  
  Totals <- as.data.frame(c(TotSkew, TotQuantile))
  
  colnames(Totals)=c("Totals")
  rownames(Totals) <- c("Skewness", paste("IBNR ", probs*100, "%:", sep=""))
  
  output <- list(ByOrigin=ByOrigin, Totals=Totals)
  return(output)
}

##############################################################################
## Calculation of the skewness (Eric Dal Moro - added 31 July 2018)
## 

Asymetrie<- function(x) {
  
  if(! ("MackChainLadder" %in% class(x))){
    stop("x is not a MackChainLadder output")
  }
  
  
  Triangle <- x$Triangle
  FullTriangle <- x$FullTriangle
  
  n <- dim(Triangle)[2]
  
  Skewnes <- c(n-1)
  Skewnesi <- c(n)
  Sk3ki <- c(n)
  Inter <- c(n-1)
  OverSkew <- c(1)
  Variance <- c(n-1)
  Correlation <- matrix(nrow=n-2,ncol=n-2) 
  
  MackModel <- x$Models
  Sigma2<-x$sigma^2
  
  f <- x$f
  f.se <- x$f.se
  sigma <- x$sigma
  
  ## Calculation of the difference between individual chain-ladder coefficients and the chain-ladder coef 
  CLRatio <- function(i, Triangle, f){
    y=Triangle[,i+1]/Triangle[,i] - f[i]
  } 
  
  myModel <- sapply(c(1:(n-1)), CLRatio, Triangle, f)
  
  Interm1<-function(i, yData){
    Interm1 <- sum(yData[c(1:(n-i)),i]^1.5)
  }
  
  Interm2<-function(i, yData){
    Interm2 <- sum(yData[c(1:(n-i)),i])
  }
  
  #Calculation of Sk3k
  
  Skew<- function(i, yModel, yData, Interme1, Interme2){
    #    yModel <- yModel[!is.na(yModel)]
    y=1/(n-i-Interme1[i]^2/Interme2[i]^3)*sum(yData[c(1:(n-i)),i]^1.5*(yModel[c(1:(n-i)),i]^3))
  } 
  
  Interme1 <- sapply(c(1:(n-1)), Interm1, Triangle)
  Interme2 <- sapply(c(1:(n-1)), Interm2, Triangle)
  Interme2<- as.numeric(Interme2)
  Sk3k <- sapply(c(1:(n-1)), Skew, myModel, Triangle, Interme1, Interme2)
  
  for (k in c(1:(n-1)))
  {
    if ((is.infinite(Sk3k[k])) | (is.nan(Sk3k[k])))
    {Sk3k[k]=0}
  }
  
  
  # Calculation of Skewness per accident year
  
  for (k in c(1:(n-1))) {
    Variance[k] <- Triangle[n+1-k,k]^2*Sigma2[k]*(1/Interme2[k]+1/Triangle[n+1-k,k])
    
    if (k<n-1) { Skewnes[k] <- Triangle[n+1-k,k]^1.5*Sk3k[k]+Triangle[n+1-k,k]^3*Sk3k[k]*Interme1[k]/Interme2[k]^3 }
    
    for (j in c((k+1):(n-1))) {
      intermediaire <- 0
      intermediaire1 <- 0
      for (v in c(1:(n-j))) {
        intermediaire <- intermediaire + FullTriangle[v,j]
      }
      
      for (v in c(1:(n-j))) {
        intermediaire1 <- intermediaire1 + FullTriangle[v,j]^1.5
      }
      
      if (k<n-1) {
        Skewnes[k] <- Skewnes[k]*f[j]^3+FullTriangle[n+1-k,j]^1.5*Sk3k[j]*(1+Variance[k]/FullTriangle[n+1-k,j]^2)^(3/8)+3*Sigma2[j]*f[j]*Variance[k]+FullTriangle[n+1-k,j]^3*intermediaire1/intermediaire^3*Sk3k[j]
      }
      if (k<n-1) {
        Variance[k] <- Variance[k]*f[j]^2+FullTriangle[n+1-k,j]^2*Sigma2[j]*(1/intermediaire+1/FullTriangle[n+1-k,j])
      }
      
    }
  }
  
  #Calculation of Mack correlation between accident years (Mack article 1993)
  for (k in c(1:(n-1))) {
    Inter[n-k]<-Sigma2[k]/f[k]^2/Interme2[k]
  }
  
  for (k in c(2:(n-2))) {
    Inter[k]<-Inter[k-1]+Inter[k]
  }
  
  for (k in c(1:(n-2))) {
    for (l in c((k+1):(n-1))) {
      Correlation[k,l-1]<-Inter[k]*FullTriangle[k+1,n]*FullTriangle[l+1,n]/Variance[n-k]^0.5/Variance[n-l]^0.5
    }
  }
  
  #Calculation of overall Skewness across all accident years
  OverSkew<-sum(Skewnes)
  
  for (o in c(1:(n-2))) {
    for (p in c((o+1):(n-1))) {
      OverSkew=OverSkew + 3*Correlation[o,p-1]*(Variance[n-o]*Variance[n-p])^0.5*(Variance[n-o]/FullTriangle[o+1,n]+Variance[n-p]/FullTriangle[p+1,n])*(2+Correlation[o,p-1]*(Variance[n-o]*Variance[n-p])^0.5/(FullTriangle[p+1,n]*FullTriangle[o+1,n]))
      OverSkew=OverSkew + 3*Correlation[o,p-1]^2*(Variance[n-o]*Variance[n-p])*(FullTriangle[o+1,n]+FullTriangle[p+1,n])/(FullTriangle[o+1,n]*FullTriangle[p+1,n])
    }
  }
  
  for (o in c(1:(n-3))) {
    for (p in c((o+1):(n-2))) {
      for (q in c((p+1):((n-1)))) {
        OverSkew=OverSkew + 6*Correlation[o,p-1]*Correlation[p,q-1]*Correlation[o,q-1]*(Variance[n-o]*Variance[n-p]*Variance[n-q])^0.5*((Variance[n-o]*Variance[n-p]*Variance[n-q])^0.5/(FullTriangle[o+1,n]*FullTriangle[p+1,n]*FullTriangle[q+1,n])+Variance[n-o]^0.5/(Correlation[p,q-1]*FullTriangle[o+1,n])+Variance[n-p]^0.5/(Correlation[o,q-1]*FullTriangle[p+1,n])+Variance[n-q]^0.5/(Correlation[o,p-1]*FullTriangle[q+1,n]))
      }
    }
  }
  
  Skewnesi[1]<-0
  Skewnesi[2]<-0
  Sk3ki[1]<-0
  Sk3ki[2]<-0
  
  for (k in c(3:n)) {
    
    Skewnesi[k]<-0
    
    if (Variance[n-k+1]>0) {
      Skewnesi[k]<-Skewnes[n-k+1]/Variance[n-k+1]^1.5
    }
    
    Sk3ki[k]<-Sk3k[n-k+1]
  }  
  
  output <- list()
  output[["Skewnes"]]<-Skewnesi
  output[["Correlation"]]<-Correlation
  output[["OverSkew"]]<-OverSkew
  output[["Sk3k"]]<-Sk3ki
  
  return(output)
}

## End addition Eric Dal Moro 31 July 2018
