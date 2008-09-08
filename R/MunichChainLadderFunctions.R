# Author: Markus Gesmann
# Copyright: Markus Gesmann, markus.gesmann@web.de
# Date:10/11/2007

####################################################
# Get the top left triangle of a matrix
left.tri <- function(x) col(as.matrix(x)) < ncol(as.matrix(x))-row(as.matrix(x)) + 2
####################################################

##############################################################################
# Munich Chain Ladder
#
MunichChainLadder <- function(Paid, Incurred){
	
# if(!all(dim(Paid) == dim(Incurred)))
# 	stop("Paid and Incurred triangle must have same dimension.\n")
# if(nrow(Paid) != ncol(Paid))
# 	stop("Number of origin years has to be equal to number of development years.\n")	
	

 MackPaid = MackChainLadder(Paid)
 MackIncurred = MackChainLadder(Incurred)

 n <- ncol(Paid)

 FullPaid = MackPaid[["FullTriangle"]]
 FullIncurred = MackIncurred[["FullTriangle"]]


 myQModel <- vector("list", n)
 q.f <- rep(1,n)
 rhoI.sigma <- rep(0,n)

 for(s in c(1:n)){
 	myQModel[[s]] <- lm(Paid[1:(n-s+1),s] ~ Incurred[1:(n-s+1),s] + 0,
					weight=1/Incurred[1:(n-s+1),s])

	q.f[s] = summary(myQModel[[s]])$coef[1]
	rhoI.sigma[s] = summary(myQModel[[s]])$sigma
 }


 myQinverseModel <- vector("list", n)
 qinverse.f <- rep(1,n)
 rhoP.sigma <- rep(0,n)

 for(s in c(1:n)){
 	myQinverseModel[[s]] <- lm(Incurred[1:(n-s+1),s] ~ Paid[1:(n-s+1),s] + 0,
					weight=1/Paid[1:(n-s+1),s])

	qinverse.f[s] = summary(myQinverseModel[[s]])$coef[1]
	rhoP.sigma[s] = summary(myQinverseModel[[s]])$sigma
 }

  # Estimate the residuals

  Paidf <-  t(matrix(rep(MackPaid$f[-n],(n-1)), ncol=(n-1)))
  PaidSigma <- t(matrix(rep(MackPaid$sigma,(n-1)), ncol=(n-1)))
  PaidRatios <- Paid[-n,-1]/Paid[-n,-n]
  PaidResiduals <- (PaidRatios - Paidf)/PaidSigma * sqrt(Paid[-n,-n])

  Incurredf <-  t(matrix(rep(MackIncurred$f[-n],(n-1)), ncol=(n-1)))
  IncurredSigma <- t(matrix(rep(MackIncurred$sigma,(n-1)), ncol=(n-1)))
  IncurredRatios <- Incurred[-n,-1]/Incurred[-n,-n]
  IncurredResiduals <- (IncurredRatios - Incurredf)/IncurredSigma * sqrt(Incurred[-n,-n])


  QRatios <- (Paid/Incurred)[,-n]
  Qf <- t(matrix(rep(q.f[-n],n), ncol=n))
  QSigma <- t(matrix(rep(rhoI.sigma[-n],n), ncol=n))
  QResiduals <- (QRatios - Qf)/QSigma * sqrt(Incurred[,-n])

  QinverseRatios <- 1/QRatios
  Qinversef <- 1/Qf
  QinverseSigma <- t(matrix(rep(rhoP.sigma[-n],n), ncol=n))
  QinverseResiduals <- (QinverseRatios - Qinversef)/QinverseSigma * sqrt(Paid[,-n])


 # linear regression of the residuals through the origin
 
 .x <- na.omit(data.frame(QinverseResiduals=QinverseResiduals[left.tri(QinverseResiduals)] , 
 						  PaidResiduals=PaidResiduals[left.tri(PaidResiduals)]))
 .x <- .x[is.finite(.x$QinverseResiduals) & is.finite(.x$PaidResiduals),]
 
 lambdaP <- coef(lm(QinverseResiduals ~ PaidResiduals + 0, data=.x))
 
 .y <- na.omit(data.frame(QResiduals=QResiduals[left.tri(QResiduals)], 
 						  IncurredResiduals=IncurredResiduals[left.tri(IncurredResiduals)]))
 .y <- .y[is.finite(.y$QResiduals) & is.finite(.y$IncurredResiduals),]
 lambdaI <- coef(lm(QResiduals ~ IncurredResiduals +0, data=.y))


 # Recursive Munich Chain Ladder Forumla
 FullPaid <- Paid
 FullIncurred <- Incurred
 for(j in c(1:(n-1))){
  for(i in c((n-j+1):n) ){
   # Paid
   mclcorrection <- lambdaP*MackPaid$sigma[j]/rhoP.sigma[j]*(
   	FullIncurred[i,j]/FullPaid[i,j]-qinverse.f[j]
   	)
   FullPaid[i,j+1] = FullPaid[i,j] * (MackPaid$f[j] + mclcorrection)
   # Incurred
   mclcorrection <- lambdaI*MackIncurred$sigma[j]/rhoI.sigma[j]*(
   	FullPaid[i,j]/FullIncurred[i,j]-q.f[j]
   	)
   FullIncurred[i,j+1] = FullIncurred[i,j] * (MackIncurred$f[j] + mclcorrection)

     }
   }

   output <- list()
   output[["Paid"]] <- Paid
   output[["Incurred"]] <- Incurred
   output[["MCLPaid"]] <- FullPaid
   output[["MCLIncurred"]] <- FullIncurred
   output[["SCLPaid"]] <- MackPaid
   output[["SCLIncurred"]] <- MackIncurred
   output[["PaidResiduals"]] <-  PaidResiduals
   output[["IncurredResiduals"]] <- IncurredResiduals
   output[["QResiduals"]] <- QResiduals
   output[["QinverseResiduals"]] <- QinverseResiduals
   output[["lambdaP"]] <- lambdaP
   output[["lambdaI"]] <- lambdaI


   class(output) <- c("MunichChainLadder", "list")

   return(output)
 
 }

##############################################################################
# summary 
#
summary.MunichChainLadder <- function(object,...){
   n <- ncol(object[["MCLPaid"]])	
   .Paid <- as.matrix(object[["Paid"]])
   .Incurred <- as.matrix(object[["Incurred"]])   

  	LatestPaid = rev(.Paid[row(.Paid) == (n+1 - col(.Paid))])
    LatestIncurred = rev(.Incurred[row(.Incurred) == (n+1 - col(.Incurred))])
    UltimatePaid = object[["MCLPaid"]][,n]
    UltimateIncurred = object[["MCLIncurred"]][,n]
   	
       Result <- data.frame(LatestPaid,	LatestIncurred, 
       				Latest.P.I.Ratio=LatestPaid/LatestIncurred,
       				UltimatePaid,
       				UltimateIncurred,
       				Ultimate.P.I.Ratio=UltimatePaid/UltimateIncurred)
	return(Result)
}

##############################################################################
# print 
#	
 print.MunichChainLadder <- function(x,...){
  	print(format(summary(x), big.mark = ",", digits = 3),...)
 }

##############################################################################
# plot 
#
 plot.MunichChainLadder <- function(x, mfrow=c(2,2), title=NULL, ...){

 if(is.null(title)) myoma <- c(0,0,0,0) else myoma <- c(0,0,2,0)

 op=par(mfrow=mfrow, oma=myoma)
 
  n <- ncol(x[["MCLPaid"]])
  barplot(t(as.matrix(data.frame(Paid=x[["MCLPaid"]][,n], Incurred=x[["MCLIncurred"]][,n]))),
  beside=TRUE, legend.text=c("MCL Paid", "MCL Incurred"), names.arg=c(1:n),
  xlab="origin year", ylab="Amounts", main="Munich Chain Ladder Results")

  barplot(t(as.matrix(
  data.frame(SCL.PI=100*x[["SCLPaid"]]$FullTriangle[,n]/x[["SCLIncurred"]]$FullTriangle[,n],
  			MCL.PI=100*x[["MCLPaid"]][,n]/x[["MCLIncurred"]][,n]))),
  beside=TRUE, legend.text=c("SCL P/I", "MCL P/I"), names.arg=c(1:n),
  xlab="origin year", ylab="%", main="Munich Chain Ladder vs. Standard Chain Ladder")


  plot(x[["PaidResiduals"]][left.tri(x[["PaidResiduals"]])] ~ x[["QinverseResiduals"]][left.tri(x[["QinverseResiduals"]])],
    xlim=c(-2,2), ylim=c(-2,2),
    xlab="Paid residuals",
    ylab="Incurred/Paid residuals",
    main="Paid residual plot")
	abline(v=0)
	abline(h=0)
	abline(a=0,b=x[["lambdaP"]], col="red")

  plot(x[["IncurredResiduals"]][left.tri(x[["IncurredResiduals"]])] ~ x[["QResiduals"]][left.tri(x[["QResiduals"]])],
    xlim=c(-2,2), ylim=c(-2,2),
    xlab="Incurred residuals",
    ylab="Paid/Incurred residuals",
    main="Incurred residual plot")
	abline(v=0)
	abline(h=0)
	abline(a=0,b=x[["lambdaI"]], col="red")

  title( title , outer=TRUE) 
  par(op)
  }