# Functions to fit multivariate chain ladder models, estimate Mse, and generate diagonostic plots
# Author: Wayne (Yanwei) Zhang 
# Email: actuaryzhang@uchicago.edu


## Define S4 classes 
##

# class of "triangles" and validation
setClass("triangles",
		representation("list")
 )

#  Validate the class "triangles" 
.valid.triangles <- function(object){
	if (!is.list(object))
		stop("Triangles must be supplied as list!\n")
    dims <- sapply(object, dim)
    if (length(dims)>0 && !all( dims - apply(dims, 1, mean) ==0)) 
		stop("Triangles do not have the same dimensions!\n")
	else TRUE
	}
	
setValidity("triangles", .valid.triangles )


# class of "MultiChainLadderFit" as virtual class
setClass("MultiChainLadderFit", 
	representation(
			Triangles="triangles",
			models="list",
			B="list",
			Bcov="list",
			ecov="list",
			fit.method="character",
			delta="numeric"),
	prototype(
			Triangles=new("triangles",list()),
			models=list(),
			B=list(),
			Bcov=list(),
			ecov=list(),
			fit.method=character(0),
			delta=1),
	contains="VIRTUAL"	
)



# class of "GMCLFit", result of call from ".FitGMCL"
setClass("GMCLFit", "MultiChainLadderFit")

# class of "MCLFit", result of call from ".FitMCL" 
setClass("MCLFit", "MultiChainLadderFit") 


# function to check if the components of B, Bcov, ecov have the same dimensions
.valid.parms <- function(object){
	if (length(object) > 0) {		
		#check for all numeric
		z=sapply(object,is.numeric)
		if (!all(z==z[1])) 
			stop("Each component should be numeric values!")
		
		# check for equal dimensions
		if ((is.vector(object[[1]]) && length(unique(sapply(object,length)))!=1) ||
			 (is.matrix(object[[1]]) && !all( sapply(object,dim)== sapply(object,dim)[,1])))
			stop("Each component must be of the same length!\n")

		}
	}
	

.valid.MultiChainLadderFit <- function(object){
	len <- c(length(object@B),length(object@Bcov),length(object@ecov))
	if (length(unique(len)) >1) 
		stop("B, Bcov and ecov must have the same length!\n")
	.valid.parms(object@B)
	.valid.parms(object@Bcov)
	.valid.parms(object@ecov)
	TRUE
	}

setValidity("GMCLFit",.valid.MultiChainLadderFit)
setValidity("MCLFit",.valid.MultiChainLadderFit)	
# class of "MultiChainLadderMse"
setClass("MultiChainLadderMse",
	representation(
			mse.ay="matrix",
			mse.ay.est="matrix",
			mse.ay.proc="matrix",
			mse.total="matrix",
			mse.total.est="matrix",
			mse.total.proc="matrix",
			FullTriangles="triangles"),
	prototype(
			mse.ay=matrix(0,0,0),
			mse.ay.est=matrix(0,0,0),
			mse.ay.proc=matrix(0,0,0),
			mse.total=matrix(0,0,0),
			mse.total.est=matrix(0,0,0),
			mse.total.proc=matrix(0,0,0),
			FullTriangles=new("triangles",list()) )
)


# class of "MultiChainLadder"
setClass("MultiChainLadder", 
	representation(model="character"),
	prototype(model=character(0)),
	contains=c("MultiChainLadderFit","MultiChainLadderMse")
)


## Define generic functions 
##


# generic function for Mse calculation
setGeneric("Mse",
           function(ModelFit, FullTriangles, ...)
           standardGeneric("Mse")
)



# generic function for residual covariance
if (!isGeneric("residCov")) {
    setGeneric("residCov",
               function(object, ...)
               standardGeneric("residCov"))
}



# generic function for calculating standard residuals
if (!isGeneric("rstandard")) {
    setGeneric("rstandard",
               function(model, ...)
               standardGeneric("rstandard"))
}


## fucntions and methods
##


MultiChainLadder <- function(Triangles,
				fit.method="SUR", 
				delta=1,                
				extrap=TRUE ,
            	mse.method="Mack" ,
				model="MCL", ...){
	
	# Convert object to class "triangles". Input data will be validated automatically. 
	Triangles  <- as(Triangles,"triangles")
	
	if (!any(fit.method %in% c("SUR", "OLS")))
		stop("Estimation method must be either SUR or OLS!\n")
		
	if (!any(mse.method %in% c("Mack", "Independence")))
		stop("Mse estimation method is not valid!\n")
		
	if (model=="GMCL" && mse.method=="Independence")
		warnings("Mse estimation under independence assumption is not available for GMCL.\n
		The Mack method is used automatically!\n",call.=FALSE)
	
	# checking of extrap is performed in calling .FitMCL
	
	
	# call .FitGMCL or .FitMCL to fit regressions	
	
	if (model %in% "GMCL"){ 
		models  <- .FitGMCL(Triangles=Triangles,
					fit.method=fit.method,
					delta=delta,...)					
	}
		
	if (model %in% "MCL") {		
		models  <- .FitMCL(Triangles=Triangles,
					fit.method=fit.method,
					delta=delta,
					extrap=extrap,...)
	}
	
	# complete triangles
	FullTriangles  <- predict(models)	 	
	
	# calculate mse
	mse.models <- Mse(ModelFit=models,
				FullTriangles=FullTriangles,
				mse.method=mse.method)

	# create an object of class "MultiChainLadder"
	output <- new("MultiChainLadder",
			Triangles=models@Triangles,
			models=models@models,
			B=models@B ,
			Bcov=models@Bcov,
			ecov=models@ecov,
			fit.method=models@fit.method,
			delta=models@delta,
			mse.ay=mse.models@mse.ay,
			mse.ay.est=mse.models@mse.ay.est,
			mse.ay.proc=mse.models@mse.ay.proc,
			mse.total=mse.models@mse.total,
			mse.total.est=mse.models@mse.total.est,
			mse.total.proc=mse.models@mse.total.proc,
			FullTriangles=mse.models@FullTriangles,
			model=model )   
			
	return(output)
}



# fit the GMCL model 
.FitGMCL <- function(Triangles,
				fit.method="SUR",
				delta=1, ...){

	p <- length(Triangles)
	m <- dim(Triangles[[1]])[1] 
	n <- dim(Triangles[[1]])[2]

	myModel <- vector("list",n-1)  #this is a list with all the fitted regressions
	B <- Bcov <- ecov <- vector("list",n-1)
	chngOrder <- as.vector(matrix(1:(p^2),p,p,byrow=TRUE)) # Change the order due to the vectorization  
		for (i in 1:(n-1)){   			
			y <- x <- system <- vector("list",p)
			for (j in 1:p){
				y[[j]] <- Triangles[[j]][1:(m-i),i+1]*(Triangles[[j]][1:(m-i),i])^(-delta/2)   
				x[[j]] <- lapply(1:p,function(x) 
							Triangles[[x]][1:(m-i),i]*(Triangles[[j]][1:(m-i),i])^(-delta/2)) 
				
				system[[j]] <- as.formula(
							paste("y[[",j,"]]~-1+",   
								paste("x[[",j,"]][[",1:p,"]]",sep="",collapse="+"),sep="")) 
			}
			
			myModel[[i]] <- systemfit(system,fit.method,...)  
			B[[i]] <-  matrix(coef(myModel[[i]]),p,p,byrow=TRUE)  
			Bcov[[i]] <- vcov(myModel[[i]])[chngOrder,chngOrder] 
			ecov[[i]] <- myModel[[i]]$residCov  
		}
	# create an object of class "GMCLFit" 
	output <- new("GMCLFit",
			Triangles=Triangles,
			models=myModel,
			B=B,
			Bcov=Bcov,
			ecov=ecov,
			fit.method=fit.method,
			delta=delta )			
	return(output)
}

# fit the MCL model 
.FitMCL <- function(Triangles,
				fit.method="SUR",
				delta=1,
				extrap=TRUE,...)
	{
	p <- length(Triangles)
	m <- dim(Triangles[[1]])[1]
	n <- dim(Triangles[[1]])[2]
	
	# if the last period has enough data to fit regression, then set extrap=FALSE automatically
	if ((sum(!is.na(Triangles[[1]][,n])) > 1) && (extrap==TRUE)) {
		warning("Trapezoids do not need exptrapolation. The value of extrap is changed to FALSE.\n", call.=FALSE)
		extrap=FALSE
		}
	
	B <- Bcov <- ecov <- myModel <- vector("list",n-1)

		for (i in 1:(n-1)){
			y <- x <- system <- vector("list",p)
			for (j in 1:p){
				y[[j]]<- Triangles[[j]][1:(m-i),i+1]*(Triangles[[j]][1:(m-i),i]^(-delta/2))
				x[[j]]<- Triangles[[j]][1:(m-i),i]*(Triangles[[j]][1:(m-i),i]^(-delta/2))
				system[[j]]=as.formula(paste("y[[",j,"]]~-1+x[[",j,"]]",sep=""))
			}
			if (!(i==n-1 & extrap!=FALSE )){
				myModel[[i]] <- systemfit(system,fit.method,...)
				B[[i]]<- as.vector(coef(myModel[[i]]))
				Bcov[[i]]<- unname(vcov(myModel[[i]]))
				ecov[[i]]<- unname(myModel[[i]]$residCov)
				if (fit.method %in% "OLS") ecov[[i]] <- diag(diag(ecov[[i]]),nrow=p)
				# replace off-diagonal elements as 0
			}
			else {
				B[[i]]<- sapply(1:p,function(x) Triangles[[x]][1,i+1]/ Triangles[[x]][1,i])
				r <- abs(ecov[[i-1]]^2/ecov[[i-2]])
				ecov[[i]]<- pmin(abs(ecov[[i-2]]),abs(ecov[[i-1]]),replace(r,is.na(r),0))
				Bcov[[i]]<- solve(diag(unlist(x),nrow=p)%*%solve(ecov[[i]])%*%diag(unlist(x),nrow=p))
				myModel[[i]]<- "Not applicable because of extrapolation"
			}
		}

	# create an object of class "MCLFit"
	output <- new("MCLFit",
			Triangles=Triangles,
			models=myModel,
			B=B,
			Bcov=Bcov,
			ecov=ecov,
			fit.method=fit.method,
			delta=delta )			
	return(output)
	
}
 


# method to predict the full triangles for "GMCLFit" object
setMethod("predict", signature="GMCLFit",
	function(object,...){
		Triangles <- object@Triangles
		B <- object@B
		p <- length(Triangles)
		m <- dim(Triangles[[1]])[1]
		n <- dim(Triangles[[1]])[2]
		FullTriangles <- Triangles

    		for (i in 1:(n-1)){
        		x <-  sapply(1:p, function(x) FullTriangles[[x]][(m-i+1):m,i])  
        		x <- matrix(as.vector(x),p,i,byrow=TRUE)
			y <- B[[i]]%*%x
			for (j in 1:p){
				FullTriangles[[j]][(m-i+1):m,(i+1)] <- y[j,]
			}
		}
		return(FullTriangles)
	}
)

# method to predict the full triangles for "MCLFit" object
setMethod("predict", signature="MCLFit",
	function(object,...){
	Triangles <- object@Triangles
	B <- object@B
	p <- length(Triangles)
	m <- dim(Triangles[[1]])[1]
	n <- dim(Triangles[[1]])[2]
	FullTriangles <- Triangles
    	for (i in 1:(n-1)){
        	x <- sapply(1:p, function(x) FullTriangles[[x]][(m-i+1):m,i])  
        	x <- matrix(x,p,i,byrow=TRUE)
			y <- diag(B[[i]],nrow=p)%*%x
			for (j in 1:p){
				FullTriangles[[j]][(m-i+1):m,(i+1)] <- y[j,]
			}
		}
	return(FullTriangles)
	}
)


# method to calculation mse for "GMCL" 
setMethod("Mse",signature(ModelFit="GMCLFit",
						FullTriangles="triangles"),
	function(ModelFit, FullTriangles, ...){

	Triangles <- ModelFit@Triangles
	p <- length(FullTriangles)
	n <- ncol(FullTriangles[[1]])
	m <- nrow(FullTriangles[[1]])
   	nAdd <- sum(!is.na(Triangles[[1]][,n]))-1  #  #rows in addition to the traditional triangle
	Ip <- diag(rep(1,p))
	B <- ModelFit@B
	Bcov <- ModelFit@Bcov
	ecov <- ModelFit@ecov
	delta <- ModelFit@delta
	
	mse.ay <- mse.ay.est <- mse.ay.proc <- matrix(0,m*p,n*p)
	mse.total <- mse.total.est <- mse.total.proc <- matrix(0,p,p*n)
	
	# recursive calcualtion of mse for single accident years
	for ( i in (2+nAdd):m)
		{
		for (k in (n+nAdd+1-i):(n-1))
			{
			yhat <- sapply(1:p,function(x) FullTriangles[[x]][i,k])
			a1 <- (p*(i-1)+1):(p*i)   # old indexes
			b1 <- (p*(k-1)+1):(p*k)
			a2 <- (p*(i-1)+1):(p*i)		# new indexes
			b2 <- (p*k+1):(p*(k+1))
			
			# process variance
			mse.ay.proc[a2,b2] <- B[[k]]%*%mse.ay.proc[a1,b1]%*%t(B[[k]])+
						diag((yhat)^(delta/2),nrow=p)%*%ecov[[k]]%*%diag((yhat)^(delta/2),nrow=p)
			# estimation variance				
			mse.ay.est[a2,b2] <- B[[k]]%*%mse.ay.est[a1,b1]%*%t(B[[k]])+
								kronecker(t(yhat),Ip)%*%Bcov[[k]]%*%kronecker(yhat,Ip)
 
 			# combined
			mse.ay[a2,b2] <- mse.ay.proc[a2,b2]+mse.ay.est[a2,b2]					
			}
		}
		
	# recursive calcualtion of mse for aggregated accident years
	for (k in 1:(n-1))
		{
			
		sumProc <- matrix(0,p,p)
		sumyhat <- rep(0,p)
		for (i in ((m+1-k):m))
			{
			yhat <- sapply(1:p,function(x) FullTriangles[[x]][i,k])
			
			sumProc <- sumProc+diag((yhat)^(delta/2),nrow=p)%*%ecov[[k]]%*%diag((yhat)^(delta/2),nrow=p)			
			sumyhat <- sumyhat+yhat	
			}
			
		b1 <- (p*(k-1)+1):(p*k)   #old index
		b2 <- (p*k+1):(p*(k+1))   #new index
		
		mse.total.proc[,b2] <- B[[k]]%*%mse.total.proc[,b1]%*%t(B[[k]])+sumProc
		
		mse.total.est[,b2] <- B[[k]]%*%mse.total.est[,b1]%*%t(B[[k]])+
							kronecker(t(sumyhat),Ip)%*%Bcov[[k]]%*%kronecker(sumyhat,Ip)
							
		mse.total[,b2] <- mse.total.proc[,b2]+mse.total.est[,b2]
		}

	output <- 	new("MultiChainLadderMse",
				mse.ay=mse.ay,
				mse.ay.est=mse.ay.est,
				mse.ay.proc=mse.ay.proc,
				mse.total=mse.total,
				mse.total.est=mse.total.est,
				mse.total.proc=mse.total.proc,
				FullTriangles=FullTriangles)

	return(output)
	}
)


setMethod("Mse",signature(ModelFit="MCLFit",
					FullTriangles="triangles"),
	function(ModelFit, FullTriangles, mse.method="Mack", ...){

	Triangles <- ModelFit@Triangles
	p <- length(FullTriangles)
	n <- ncol(FullTriangles[[1]])
	m <- nrow(FullTriangles[[1]])
	B <- ModelFit@B
	Bcov <- ModelFit@Bcov
	ecov <- ModelFit@ecov
	delta <- ModelFit@delta

	mse.ay <- mse.ay.est <- mse.ay.proc <- matrix(0,m*p,n*p)
	mse.total <- mse.total.est <- mse.total.proc <- matrix(0,p,p*n)


	# mse for single accident years
	for ( k in 1:(n-1))
		{
		for (i in m:(m+1-k))
			{
			a1 <- (p*(i-1)+1):(p*i)	 #old indexes
			b1 <- (p*(k-1)+1):(p*k)
			a2 <- (p*(i-1)+1):(p*i)	# new indexes
			b2 <- (p*k+1):(p*(k+1))

			yhat <- sapply(1:p,function(x) FullTriangles[[x]][i,k])
			#process variance
			mse.ay.proc[a2,b2] <- diag(yhat^(delta/2),nrow=p)%*%ecov[[k]]%*%diag(yhat^(delta/2),nrow=p)+
								B[[k]]%*%t(B[[k]])*mse.ay.proc[a1,b1]

			#estimation variance
			if (mse.method %in% "Mack")	#Mack formulas
				{
				mse.ay.est[a2,b2] <- Bcov[[k]]*(yhat%*%t(yhat))+
									(B[[k]]%*%t(B[[k]]))*mse.ay.est[a1,b1]
				}
			if (mse.method %in% "Independence") #Murphy & BBMW formulas
				{
				mse.ay.est[a2,b2] <- Bcov[[k]]*(yhat%*%t(yhat))+
									(B[[k]]%*%t(B[[k]]))*mse.ay.est[a1,b1]+
									Bcov[[k]]*mse.ay.est[a1,b1]
				}
			#mse
			mse.ay[a2,b2] <- mse.ay.proc[a2,b2]+mse.ay.est[a2,b2]
			}
		}

	# mse for aggregated accident years
	for (k in 1:(n-1))
		{

		b1 <- (p*(k-1)+1):(p*k)
		b2 <- (p*k+1):(p*(k+1))
		proc <- matrix(0,p,p)

		#process variance
		for (i in ((m+1-k):m))
			{
			yhat <- sapply(1:p,function(x) FullTriangles[[x]][i,k])
			proc <- proc+diag(yhat^(delta/2),nrow=p)%*%ecov[[k]]%*%diag(yhat^(delta/2),nrow=p)
			}

		sumyhat <- sapply(1:p,function(x) sum(FullTriangles[[x]][(m+1-k):m,k]))
		mse.total.proc[,b2] <- proc+B[[k]]%*%t(B[[k]])*mse.total.proc[,b1]

		#estimation variance
		if (mse.method %in% "Mack")
			{
			mse.total.est[,b2] <- Bcov[[k]]*(sumyhat%*%t(sumyhat))+
									(B[[k]]%*%t(B[[k]]))*mse.total.est[,b1]
			}
		if (mse.method %in% "Independence")
			{
			mse.total.est[,b2] <- Bcov[[k]]*(sumyhat%*%t(sumyhat))+
									(B[[k]]%*%t(B[[k]]))*mse.total.est[,b1]+
									Bcov[[k]]*mse.total.est[,b1]
			}

		#total variance
		mse.total[,b2] <- mse.total.proc[,b2]+mse.total.est[,b2]
		}

	# create an object of class "MCLMse"
	output <- new("MultiChainLadderMse",
				mse.ay=mse.ay,
				mse.ay.est=mse.ay.est,
				mse.ay.proc=mse.ay.proc,
				mse.total=mse.total,
				mse.total.est=mse.total.est,
				mse.total.proc=mse.total.proc,
				FullTriangles=FullTriangles)
	return(output)
	}
)



# method for summary with signature "MultiChainLadder". 
# portfolio can be used to calculate the sum of two triangles. If NULL, then all triangles will be summed.

setMethod("summary", signature="MultiChainLadder",
	function(object,portfolio=NULL,...){

	Triangles <- object@Triangles
	FullTriangles <- object@FullTriangles
	p <- length(Triangles)
	m <- dim(Triangles[[1]])[1]
	n <- dim(Triangles[[1]])[2]
	nAdd <- sum(!is.na(Triangles[[1]][,n]))-1

	if (is.null(portfolio)) portfolio <- 1:p 
		else portfolio <- as.numeric(unlist(strsplit(portfolio,"\\+",perl=TRUE)))
	
	Latest <- lapply(1:p, function(x) {
		c(rev(Triangles[[x]]
		[row(as.matrix(Triangles[[x]]))
		==(m+1-col(as.matrix(Triangles[[x]])))])[-1])})
	Latest <- lapply(1:p, function(x){
		c(Latest[[x]],sum(Latest[[x]]))
		})
	
	Ultimate <- lapply(1:p, function(x){
		c(FullTriangles[[x]][-(1:(nAdd+1)),n])
		})
	Ultimate <- lapply(1:p, function(x){
		c(Ultimate[[x]],sum(Ultimate[[x]]))
		})	

	s <- (nAdd+1)*p
	
	mse.ay.ult <- object@mse.ay[-(1:s),((n-1)*p+1):(p*n)]
	mse.ay.ult.est <- object@mse.ay.est[-(1:s),((n-1)*p+1):(p*n)]
	mse.ay.ult.proc <- object@mse.ay.proc[-(1:s),((n-1)*p+1):(p*n)]
	mse.total.ult <- object@mse.total[,((n-1)*p+1):(p*n)]
	mse.total.ult.est <- object@mse.total.est[,((n-1)*p+1):(p*n)]
	mse.total.ult.proc <- object@mse.total.proc[,((n-1)*p+1):(p*n)]
	
	if (p==1){
		Latest <- unlist(Latest)
		Ultimate <- unlist(Ultimate)
		IBNR <- Ultimate-Latest
		Dev.To.Date <-  Latest/Ultimate
		mse.ult <- c(mse.ay.ult,mse.total.ult)
		mse.ult.est <- c(mse.ay.ult.est,mse.total.ult.est)
		mse.ult.proc <- c(mse.ay.ult.proc,mse.total.ult.proc)
		S.E <- round(sqrt(mse.ult),2)
		S.E.Est <- round(sqrt(mse.ult.est),2)
		S.E.Proc <- round(sqrt(mse.ult.proc),2)	
		CV <- S.E/IBNR
		output <- data.frame(Latest, Dev.To.Date, Ultimate, IBNR, S.E, CV, S.E.Est, S.E.Proc)
		rownames(output) <- c(as.character((nAdd+2):m),"Total")
		output <- list(output)
		names(output) <- "Summary Statistics for the Input Triangle"
		}	

	else {
		Latest[[p+1]] <- Reduce("+",Latest[portfolio])
		Ultimate[[p+1]] <- Reduce("+",Ultimate[portfolio])
		IBNR <- lapply(1:(p+1),function(x) Ultimate[[x]]-Latest[[x]])
		Dev.To.Date <- lapply(1:(p+1),function(x) Latest[[x]]/Ultimate[[x]])
		mse.ult.matrix <- rbind(mse.ay.ult,mse.total.ult)
		mse.ult <- lapply(1:p, function(x) 
						mse.ult.matrix[((1:(dim(mse.ult.matrix)[1]/p)-1)*p+x),x])
		mse.ult[[p+1]] <- sapply(1:(dim(mse.ult.matrix)[1]/p), function(x) 
						sum(mse.ult.matrix[(portfolio+(x-1)*p),portfolio]))
							
		mse.ult.est.matrix <- rbind(mse.ay.ult.est,mse.total.ult.est)
		mse.ult.est <- lapply(1:p, function(x) 
						mse.ult.est.matrix[((1:(dim(mse.ult.est.matrix)[1]/p)-1)*p+x),x])
		mse.ult.est[[p+1]] <- sapply(1:(dim(mse.ult.est.matrix)[1]/p), function(x) 
						sum(mse.ult.est.matrix[(portfolio+(x-1)*p),portfolio]))
							
		mse.ult.proc.matrix <- rbind(mse.ay.ult.proc,mse.total.ult.proc)
		mse.ult.proc <- lapply(1:p, function(x) 
						mse.ult.proc.matrix[((1:(dim(mse.ult.proc.matrix)[1]/p)-1)*p+x),x])
		mse.ult.proc[[p+1]] <- sapply(1:(dim(mse.ult.proc.matrix)[1]/p), function(x) 
						sum(mse.ult.proc.matrix[(portfolio+(x-1)*p),portfolio]))														
		S.E <- lapply(1:(p+1), function(x) sqrt(mse.ult[[x]]))
		S.E.Est <- lapply(1:(p+1), function(x) sqrt(mse.ult.est[[x]]))
		S.E.Proc <- lapply(1:(p+1), function(x) sqrt(mse.ult.proc[[x]]))
		
		CV <- lapply(1:(p+1), function(x) S.E[[x]]/IBNR[[x]])
		output <- lapply(1:(p+1), function(x) {
						output <- data.frame(Latest=Latest[[x]], 
								Dev.To.Date=round(Dev.To.Date[[x]],4), 
								Ultimate=Ultimate[[x]], 
								IBNR=IBNR[[x]], 
								S.E=round(S.E[[x]],2), 
								CV=round(CV[[x]],4),
								S.E.Est=round(S.E.Est[[x]],2),
								S.E.Proc=round(S.E.Proc[[x]],2))
						rownames(output) <- c(as.character((nAdd+2):m),"Total")
						return(output)
						})
		nm <- paste("Summary Statistics for Triangle",
			c(as.character(1:p), paste(portfolio,sep="",collapse="+")),sep=" ")
		names(output) <- nm
		}
	
	return(output)  #output is a list  
	}
)


setMethod("print","MultiChainLadder",
	function(x,...){	
    summary.x <- summary(x)
	p <-  length(x@Triangles)
   
	for (i in 1:length(summary.x)){
		nm <- names(summary.x)[i]
		cat(nm)
		cat("\n")
		print(format(summary.x[[i]], big.mark = ",", digits = 3),...)
		}
	}
)

setMethod("show",
    signature(object = "MultiChainLadder"),
    function (object) 
    {
          summary.x <- summary(object)
	p <-  length(object@Triangles)
   
	for (i in 1:length(summary.x)){
		nm <- names(summary.x)[i]
		cat(nm)
		cat("\n")
		print(format(summary.x[[i]], big.mark = ",", digits = 3))
		}
	}
    
)


# This function joins two pieces of one object of "triangles" together. The input
# triangles should be result of "[" for class "triangles" designed to fit
# different models for different periods. This function is used internally by "Join2Fits".

.Join2Triangles <- function(triangles1,triangles2){
	# triangles1 must come from the first several developments 
	if (nrow(triangles1[[1]])< nrow(triangles2[[1]]))
		stop("The first object must have more rows!\n")

	n1=dim(triangles1[[1]])[1]
	m1=dim(triangles1[[1]])[2]
	n2=dim(triangles2[[1]])[1]
	m2=dim(triangles2[[1]])[2]
	
	.triangles1 <- triangles1[,m1]
	.triangles2 <- triangles2[,1]
	
	# check to see if the two triangles can be joined 
	
	if (!all.equal(do.call("rbind",.triangles1), do.call("rbind",.triangles2)))
		stop("The two triangles can not be joined!\n 
		Make sure the last columns of the first triangles agree with the 
		first columns of the second triangels!\n")
		
	t2=triangles2[,2:m2]
	na=matrix(NA,n1-n2+1,m2-1)
	t2=lapply(t2,rbind,na)	
	Triangles=lapply(1:length(triangles1), function(x) cbind(triangles1[[x]],t2[[x]]))

	Triangles=as(Triangles,"triangles")
	return(Triangles)
}

# transform the Bcov from MCL to GMCL format, used internally by Join2Fits
.Bcov <- function(Bcov.orig){
	p <- nrow(Bcov.orig)
	Bcov.transf <- matrix(0,p^2,p^2)
	b <- p*(1:p-1)+1:p
	for (k in b) {
		for (j in b){
			pos1 <- which(b==k)
			pos2 <- which(b==j)
			Bcov.transf[k,j] <- Bcov.orig[pos1,pos2]
		}
	}
	return(Bcov.transf)
}


# This function joins two models fitted in two different periods, and returns an
# object of either "MCLFit" or "GMCLFit". If two "MCL" models are joined, the output
# is "MCLFit". If one "MCL" and one "GMCL" is joined, the output is "GMCLFit".
# The join of two "GMCL" models is of course a "GMCLFit".


Join2Fits <- function (object1, object2 ){
	
	if (class(object1)!="MultiChainLadder" || class(object2)!="MultiChainLadder")
		stop("Both objects must be of class MultiChainLadder!\n")
		
	# object1 must come from the first several developments 
	if (nrow(object1@Triangles[[1]])< nrow(object2@Triangles[[1]]))
		stop("The first object must have more rows!\n")

	if (object1@delta != object2@delta) 
		stop("The deltas must be of the same value!\n")

	model=c(object1@model,object2@model)
	
	# construct an MCLFit object 
	if (all(model=="MCL")) {
		output <- new("MCLFit", 
			Triangles=.Join2Triangles(object1@Triangles,object2@Triangles),
			models=c(object1@models,object2@models),
			B=c(object1@B,object2@B) ,
			Bcov=c(object1@Bcov,object2@Bcov),
			ecov=c(object1@ecov,object2@ecov),
			fit.method=c(object1@fit.method,object2@fit.method),
			delta=object1@delta)
	}

	if (any(model=="GMCL") && any(model=="MCL")) {
		# assume the first is GMCL and the latter is MCL
		output <- new("GMCLFit", 
			Triangles=.Join2Triangles(object1@Triangles,object2@Triangles),
			models=c(object1@models,object2@models), 
			B=c(object1@B,lapply(object2@B,diag)) , 
			Bcov=c(object1@Bcov,lapply(object2@Bcov,.Bcov)),
			ecov=c(object1@ecov,object2@ecov),
			fit.method=c(object1@fit.method,object2@fit.method),
			delta=object1@delta)
	}

	if (all(model=="GMCL")) {

		output <- new("GMCLFit", 
			Triangles=.Join2Triangles(object1@Triangles,object2@Triangles),
			models=c(object1@models,object2@models),
			B=c(object1@B,lapply(object2@B,diag)) ,
			Bcov=c(object1@Bcov,lapply(object2@Bcov,.Bcov)),
			ecov=c(object1@ecov,object2@ecov),
			fit.method=c(object1@fit.method,object2@fit.method),
			delta=object1@delta)
	}
   
	return(output)
}





# This function joins a fit object with an Mse object to construct an MultiChainLadder object
# according to the class of the fit object. 

JoinFitMse <- function(models,mse.models){
	
	if (class(models)!="MCLFit" && class(models)!="GMCLFit")
		stop("models must be of class MCLFit or GMCLFit!\n")
	if(class(mse.models)!="MultiChainLadderMse")
		stop("mse.models must be of class MultiChainLadderMSE!\n")
		
	if (class(models)=="MCLFit") model="MCL"
	if (class(models)=="GMCLFit") model="GMCL"
	
	output <- new("MultiChainLadder", 
			Triangles=models@Triangles,
			models=models@models,
			B=models@B ,
			Bcov=models@Bcov,
			ecov=models@ecov,
			fit.method=models@fit.method,
			delta=models@delta,
			mse.ay=mse.models@mse.ay,
			mse.ay.est=mse.models@mse.ay.est,
			mse.ay.proc=mse.models@mse.ay.proc,
			mse.total=mse.models@mse.total,
			mse.total.est=mse.models@mse.total.est,
			mse.total.proc=mse.models@mse.total.proc,
			FullTriangles=mse.models@FullTriangles ,
			model=model) 
	return(output)

}

# method for "as" to convert list to "tirangles"
setAs("list","triangles", function(from) {
							from2 <- lapply(from, as.matrix)
							new("triangles",from2)
							} )

# method to extract certain columns and remove NAs
setMethod("[", signature(x = "triangles", i = "missing", j = "numeric",
			 drop = "missing"),
	function (x, i, j, ..., drop) {
		output <- lapply(x,function(y) y[!apply(as.matrix(is.na(y[,j])),1,all),j])
		output <- lapply(output,as.matrix)
		as(output,"triangles")
	}
)


setMethod("$",
    signature(x = "MultiChainLadder"),
    function (x, name) 
    {
        slot(x,name)
    }
)

setMethod("[[",
    signature(x = "MultiChainLadder",i="numeric",j="missing"),
    function (x, i, j, ...) 
    {
	  output <- lapply(i, function(y) slot(x,names(x)[y]))
        names(output) <- names(x)[i]
	  return(output)
    }
)

setMethod("[[",
    signature(x = "MultiChainLadder",i="character",j="missing"),
    function (x, i, j, ...) 
    {
	  output <- lapply(1:length(i), function(y) slot(x,i[y]))
      names(output) <- i
	  return(output)
    }
)

setMethod("names",
    signature(x = "MultiChainLadder"),
    function (x) 
    {
        slotNames(x)
    }
)


setMethod("coef",
	signature(object = "MultiChainLadder"),
    function (object,...) 
    {
	object@B
    }
)

# variance-covariance matrix. Note the definition in GMCL.
setMethod("vcov",
	signature(object = "MultiChainLadder"),
    function (object,...) 
    {
	object@Bcov
    }
)

# method to extract residual covariance
setMethod("residCov",
    signature(object = "MultiChainLadder"),
    function (object, ...) 
    {
	object@ecov
    }
)

# method for model.matrix
if (FALSE){
setMethod("model.matrix",
    signature(object = "MultiChainLadder"),
    function (object, ...) 
    {
	p <- length(object@Triangles)
	delta <- object@delta
	models <- object@models
	cl <- sapply(models,class)
    	K <- length(cl)
	if (cl[K]!= "systemfit") n <- K-1 else n <- K
	mm <- lapply(1:n, function(x) model.matrix(models[[x]]))
	if (cl[K]!= "systemfit") {
		mm2 <- sapply(object@Triangles, "[", 1, K)^(delta/2)
		mm2 <- list(diag(mm2,nrow=p))
		mm <- c(mm,mm2)
		}
	return(mm)
	
    }
)
}


# method to extract residuals, on the model-fit level, not the original scale
# should use the standardized residuals, which is independent of scale

setMethod("residuals",
    signature(object = "MultiChainLadder"),
    function (object, ...) 
    {
	p <- length(object@Triangles)
	models <- object@models
	cl <- sapply(models,class)
    	K <- length(cl)
	if (cl[K]!= "systemfit") n <- K-1 else n <- K
	r <- lapply(1:n, function(x) residuals(models[[x]]))
	if (cl[K]!= "systemfit") {
		rl <- as.data.frame(matrix(0,1,p))
		names(rl) <- names(r[[n]])
		r <- c(r,list(rl))
		}
	return(r)
    }
)


# method to calculate standard residuals
setMethod("rstandard",
    signature(model = "MultiChainLadder"),
    function (model, ...) 
    {
	r <- residuals(model)
	ecov <- residCov(model)
	lapply(1:length(r), function(x) {
					s <- sqrt(diag(ecov[[x]]))
					sweep(r[[x]],2,s,"/") 
				})				
    }
)

# generate fitted values on the original scale
setMethod("fitted",
    signature(object = "MultiChainLadder"),
    function (object,...) 
    {
	model=object@model
	Triangles <- object@Triangles
	B <- object@B
	p <- length(Triangles)
	m <- dim(Triangles[[1]])[1]
	n <- dim(Triangles[[1]])[2]
	fitted=vector("list",n-1)

	if (model=="GMCL") {
    		for (i in 1:(n-1)){
        		x <- sapply(Triangles, "[", 1:(m-i),i)  
      		fitted[[i]] <- x%*%B[[i]]
		}
	}
	if (model=="MCL") {
    		for (i in 1:(n-1)){
        		x <- sapply(Triangles, "[", 1:(m-i),i)  
      		fitted[[i]] <- x%*%diag(B[[i]],nrow=p)
		}
	}

	return(fitted)
    }
)



setMethod("fitted.values",
    signature(object = "MultiChainLadder"),
    function (object,...) 
    {
		fitted(object)
    }
)    


setMethod("plot",
    signature(x = "MultiChainLadder",y="missing"),
    function (x, y, which.plot=1:4, which.triangle=NULL, main=NULL,  portfolio=NULL,lowess=TRUE, legend.cex=0.75,...) 
    {
    	
    p <- length(x@Triangles)
    if (is.null(which.triangle)) which.triangle <- if (p>1) 1:(p+1) else 1	
    if (!is.numeric(which.triangle) || any(which.triangle < 0) || any(which.triangle > ifelse(p>1,p+1,1)))
    	stop("The value of which.triangle is not valid!\n")
    	
    if (!is.numeric(which.plot) || any(which.plot < 1) || any(which.plot > 4)) 		stop("The value of which.plot must be in 1:4!\n")
    	
	if (!is.null(main) && (!is.list(main) || length(main)!=4))
		stop("main must be a list of length 4!\n")	
	
		
		.myResult <-   summary(x,portfolio=portfolio) 
		n <-  nrow(.myResult[[1]]) 
		
		if (any(which.plot==1)){
			for (i in which.triangle){
				if (!is.null(main)) main2 <- main[[1]][i] else main2 <- if (i <=p) paste("Barplot for Triangle",i) else "Portfolio"
				
        		plotdata <- t(as.matrix(.myResult[[i]][-n,c("Latest","IBNR")]))
        		
				ymax <- max(apply(.myResult[[i]][-n,c("Ultimate", "S.E")],1,sum),na.rm=TRUE)
				
				bp <- barplot(plotdata,
                      			names.arg=rownames(.myResult),
                      			main=main2,
                      			xlab="Origin period",
                      			ylab="Value",
                      			ylim=c(0, 1.25*ymax),...)
						
				legend("topleft",c("Latest","Forecast"),fill=gray.colors(2),inset=c(0.1,0.1),cex=legend.cex)

       			errbar(x=bp, 
							y=.myResult[[i]][-n,"Ultimate"],
               				yplus=(.myResult[[i]][-n,"Ultimate"]+ .myResult[[i]][-n,"S.E"]),
               				yminus=(.myResult[[i]][-n,"Ultimate"] - .myResult[[i]][-n,"S.E"]),
               				cap=0.05,
               				add=TRUE)       
			}
		}

		if (any(which.plot==2)) {

			.Triangles <- x@Triangles
			.FullTriangles <- x@FullTriangles
			
			if (p>1) {
				.Triangles[[p+1]] <- Reduce("+",.Triangles)  
				.FullTriangles[[p+1]] <- Reduce("+",.FullTriangles)  
			}
			
			for (i in which.triangle){
				if (!is.null(main)) main2 <- main[[2]][i]
				else main2 <- if (i <=p) paste("Development Pattern for Triangle",i) else "Portfolio"
				
				matplot(t(.FullTriangles[[i]]), 
								t="l",
                				main=main2,
                				xlab="Development period",
                				ylab="Amount",...)
        			
				matplot(t(.Triangles[[i]]), add=TRUE)
			}
		}

		if (any(which.plot==3) || any(which.plot==4)){
			r <- rstandard(x)
			r <- do.call("rbind",r)
			fitted <-  fitted(x)
			fitted <- do.call("rbind",fitted)
			which.triangle <- which.triangle[which(which.triangle!=(p+1))]  # can not plot portfolio residuals
			
			if (any(which.plot==3)){ 
				for (i in which.triangle){
					if (!is.null(main)) main2 <- main[[3]][i]
					else main2 <- paste("Residual Plot for Triangle", i)

					plot(fitted[,i],r[,i],main=main2,
						ylab="Standardised residuals", xlab="Fitted",cex=0.75,...)
					if (lowess) lines(lowess(fitted[,i], r[,i]), col="red")
        			abline(h=0, col="grey")
				}
			}			
			
			if (any(which.plot==4)){ 
				for (i in which.triangle){
					if (!is.null(main)) main2 <- main[[4]][i]
					else main2 <- paste("QQ-Plot for Triangle", i)

					qqnorm(r[,i],main=main2,cex=0.75,...)
        			abline(0,1)
				}
			}		
		}	
	}	
)









