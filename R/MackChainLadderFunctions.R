
# Author: Markus Gesmann
# Copyright: Markus Gesmann, markus.gesmann@gmail.com
# Date:10/11/2007
# Date:08/09/2008

MackChainLadder <- function(Triangle, weights=1/Triangle){

  n <- ncol(Triangle)
  m <- nrow(Triangle)

if(n!=m){
  print(dim(Triangle))	
  stop("Number of origin years has to be equal to number of development years.\n")	
}
  myModel <- vector("list", (n-1))
  for(i in c(1:(n-1))){
  	# weighted linear regression through origin
    x <- Triangle[1:(m-i),i]
   	y <- Triangle[1:(m-i),i+1]
      	
  	myModel[[i]] <- lm(y~x+0, weights=weights[1:(m-i),i], data=data.frame(x,y))   	
 	 }
 	 
 	 # Predict the chain ladder model
   FullTriangle <- predict.TriangleModel(list(Models=myModel, Triangle=Triangle))
   
   # Estimate the standard error
   StdErr <- Mack.S.E(myModel, FullTriangle)
  
  #Collect the output                  
  output <- list()
  
  output[["Triangle"]] <- Triangle
  output[["FullTriangle"]] <- FullTriangle
  output[["Models"]] <- myModel
  output[["f"]] <- StdErr$f
  output[["f.se"]] <- StdErr$f.se
  output[["F.se"]] <- StdErr$F.se 
  output[["sigma"]] <- StdErr$sigma
  output[["Mack.S.E"]] <- StdErr$FullTriangle.se
  output[["Total.Mack.S.E"]] <- TotalMack.S.E(FullTriangle, StdErr$f, StdErr$f.se, StdErr$F.se)  
  
  class(output) <- c("MackChainLadder", "TriangleModel", "list")
  return(output)
}

###############################################################################
# predict
#	
predict.TriangleModel <- function(object,...){
  n <- ncol(object[["Triangle"]])
  m <- nrow(object[["Triangle"]])
  FullTriangle <- object[["Triangle"]]
  for(j in c(1:(n-1))){
    	for(k in c((n-j+1):m)){
      	FullTriangle[k, j+1] <- predict(object[["Models"]][[j]], 
                                 newdata=data.frame(x=FullTriangle[k, j]),...)                              	
       }
  }                               
  return(FullTriangle)
}

##############################################################################
# Calculation of the mean squared error and standard error
# mean squared error = stochastic error (process variance) + estimation error
# standard error = sqrt(mean squared error)
 
 Mack.S.E <- function(MackModel, FullTriangle, loglinear=TRUE){
   n <- ncol(FullTriangle)
   m <- nrow(FullTriangle)
   f <- rep(1,n)
   f.se <- rep(0,n)
   sigma <- rep(0,(n-1))

   # Extract estimated slopes, std. error and sigmas
   f[1:(n-1)] <- sapply(MackModel, function(x) summary(x)$coef["x","Estimate"])
   f.se[1:(n-1)] <- sapply(MackModel, function(x) summary(x)$coef["x","Std. Error"])
   sigma[1:(n-1)] <- sapply(MackModel, function(x) summary(x)$sigma)
   
  
 if(loglinear){
    # estimate sigma[n-1] via log-linear regression
    dev=c(1:(n-2))
    gn <- which(sigma>0)
    .sigma <- sigma[gn]
    .dev <- c(1:n)[gn] #dev[gn]
    sigmaModel <- lm(log(.sigma) ~ .dev)
    sigma[-gn] <- exp(predict(sigmaModel,
                              newdata=data.frame(.dev=c(1:(n-1))[-gn])))
    f.se[-gn] = sigma[n-1]/sqrt(FullTriangle[1,-gn]) 
}else{
    sigma[n - 1] <- sqrt(abs(min((sigma[n - 2]^4/sigma[n - 
              3]^2), min(sigma[n - 3]^2, sigma[n - 2]^2))))
    f.se[n-1] = sigma[n-1]/sqrt(FullTriangle[1,n-1]) 
}
  
  
  F.se <- t(t(1/sqrt(FullTriangle)[,-n])*(sigma))
  
  FullTriangle.se <- FullTriangle * 0
  # Recursive Formula
  for(i in 2:m){
    for(k in c((n+1-i):(n-1))){
    	 FullTriangle.se[i,k+1] = sqrt(
    	   FullTriangle[i,k]^2*(F.se[i,k]^2+f.se[k]^2) # 
    	   + FullTriangle.se[i,k]^2*f[k]^2
    	 )
    	}	
  	}
  return(list(sigma=sigma, f=f, f.se=f.se, F.se=F.se, FullTriangle.se=FullTriangle.se) )
  }

################################################################################
## Total reserve SE

TotalMack.S.E <- function(FullTriangle,f, f.se, F.se){

  C <- FullTriangle
  n <- ncol(C)
  m <- nrow(C)
  
  total.seR <- 0*c(1:(n))
  
  for(k in c(1:(n-1))){
    total.seR[k+1] <- sqrt(total.seR[k]^2 * f[k]^2 +
                           sum(C[c((m+1-k):m),k]^2 *
                               (F.se[c((m+1-k):m),k]^2),na.rm=TRUE)
                           + sum(C[c((m+1-k):m),k],na.rm=TRUE)^2 * f.se[k]^2 )
  }
  return(total.seR[length(total.seR)])
}


##############################################################################
# Summary
#
summary.MackChainLadder <- function(object,...){
# Summarise my results
  .Triangle <- object[["Triangle"]]
  n <- ncol(.Triangle)
  m <- nrow(.Triangle)
  
  Latest <- rev(.Triangle[row(as.matrix(.Triangle)) == (m+1 - col(as.matrix(.Triangle)))])  
  Ultimate <- object[["FullTriangle"]][,n]
  Dev.To.Date <- Latest/Ultimate
  IBNR <- Ultimate-Latest
  Mack.S.E <- object[["Mack.S.E"]][,n]  
  CoV <- Mack.S.E/(Ultimate-Latest)

  myResult <- data.frame(Latest, Dev.To.Date, Ultimate, IBNR, Mack.S.E, CoV) 

  return(myResult)
}

##############################################################################
# print 
#
print.MackChainLadder <- function(x,...){

 res <- summary(x)
 print(format(res[!is.na(res$Latest),], big.mark = ",", digits = 3),...)
 Totals <-  c(sum(res$Latest,na.rm=TRUE), sum(res$Ultimate,na.rm=TRUE),
               sum(res$IBNR,na.rm=TRUE), x[["Total.Mack.S.E"]],
               x[["Total.Mack.S.E"]]/sum(res$IBNR,na.rm=TRUE)*100)
  Totals <- formatC(Totals, big.mark=",",digit=0,format="f")
  Totals <- as.data.frame(Totals)
  colnames(Totals)=c("Totals:")
  rownames(Totals) <- c("Sum of Latest:","Sum of Ultimate:",
                        "Sum of IBNR:","Total Mack S.E.:",
                        "Total CoV:")
  cat("\n")
  print(Totals, quote=FALSE)

	}


################################################################################
# plot
#
plot.MackChainLadder <- function(x, mfrow=c(3,2), title=NULL,...){

 if(is.null(title)) myoma <- c(0,0,0,0) else myoma <- c(0,0,2,0)

 op=par(mfrow=mfrow, oma=myoma)

  .myResult <-  summary(x) 
  
  .FullTriangle <- x[["FullTriangle"]]
  .Triangle <- x[["Triangle"]]
  n <- nrow(.Triangle)
  bp <- barplot(t(as.matrix(.myResult[,c("Latest","IBNR")])), 
  		legend.text=c("Latest","IBNR"),
  		names.arg=c(1:n),
  		main="Mack Chain Ladder Results",
  		xlab="Origin year",
  		ylab="Amounts",#paste(Currency,myUnit), 
  		ylim=c(0, max(apply(.myResult[c("Ultimate", "Mack.S.E")],1,sum),na.rm=TRUE)))
  		
  # add error ticks
  require("Hmisc")		
  errbar(x=bp, y=.myResult$Ultimate, 
  	yplus=(.myResult$Ultimate + .myResult$Mack.S.E), 
  	yminus=(.myResult$Ultimate - .myResult$Mack.S.E),
  	cap=0.05,
  	add=TRUE)
  
  matplot(t(.FullTriangle), t="l", 
    main="Chain ladder developments by origin year",
    xlab="Development year", 
    ylab="Amounts", #paste(Currency, myUnit)
    )
  matplot(t(.Triangle), add=TRUE)
  
  Residuals=residuals(x)
    plot(standard.residuals ~ fitted.value, data=Residuals,
    	ylab="Standardised residuals", xlab="Fitted")
    	lines(lowess(Residuals$fitted.value, Residuals$standard.residuals), col="red")
    	abline(h=0, col="grey") 
    plot(standard.residuals ~ origin.year, data=Residuals,
    	ylab="Standardised residuals", xlab="Origin year")
    	lines(lowess(Residuals$origin.year, Residuals$standard.residuals), col="red")
    	abline(h=0, col="grey")
    plot(standard.residuals ~ cal.year, data=Residuals,
    	ylab="Standardised residuals", xlab="Calendar year")
    	lines(lowess(Residuals$cal.year, Residuals$standard.residuals), col="red")
      abline(h=0, col="grey")
    plot(standard.residuals ~ dev.year, data=Residuals,
    	ylab="Standardised residuals", xlab="Development year")
    	lines(lowess(Residuals$dev.year, Residuals$standard.residuals), col="red")
  	abline(h=0, col="grey")

  title( title , outer=TRUE) 
  par(op)
}
################################################################################
# residuals
#
residuals.MackChainLadder <- function(object,...){
   n <- nrow(object[["Triangle"]])
   myresiduals <- unlist(lapply(object[["Models"]], resid,...))
   standard.residuals <- unlist(lapply(object[["Models"]], rstandard,...))
   fitted.value <- unlist(lapply(object[["Models"]], fitted)) 
   origin.year <- unlist(lapply(1:(n-1), function(x) 1:length(resid( object[["Models"]][[x]]) )))
   dev.year <- unlist(lapply(1:(n-1), function(x) rep(x,length(resid( object[["Models"]][[x]]) ))))
   cal.year <- origin.year + dev.year - 1
   
   myResiduals=data.frame(origin.year,
                          dev.year,
                          cal.year,
                          residuals=myresiduals, 
                          standard.residuals,
                          fitted.value)
   return(na.omit(myResiduals))
   }
 

