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

# virtual class for representation
setClassUnion("NullChar",c("NULL","character"))	


# class of "MultiChainLadderFit" as virtual class
setClass("MultiChainLadderFit", 
	representation(
			Triangles="triangles",
			models="list",
			coefficients="list",
			coefCov="list",
			residCov="list",
			fit.method="character",
			delta="numeric",
			int="NullNum",
			restrict.regMat="NullList"),
	prototype(
			Triangles=new("triangles",list()),
			models=list(),
			coefficients=list(),
			coefCov=list(),
			residCov=list(),
			fit.method=character(0),
			delta=1,
			int=NULL,
			restrict.regMat=NULL),
	contains="VIRTUAL"	
)



# class of "GMCLFit", result of call from ".FitGMCL"
setClass("GMCLFit", "MultiChainLadderFit")

# class of "MCLFit", result of call from ".FitMCL" 
setClass("MCLFit", "MultiChainLadderFit") 

# not used now since the new function allows different structure to be combined
if (FALSE){
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
	len <- c(length(object@coefficients),length(object@coefCov),length(object@residCov))
	if (length(unique(len)) >1) 
		stop("coefficients, coefCov and residCov must have the same length!\n")
	.valid.parms(object@coefficients)
	.valid.parms(object@coefCov)
	.valid.parms(object@residCov)
	TRUE
	}

setValidity("GMCLFit",.valid.MultiChainLadderFit)
setValidity("MCLFit",.valid.MultiChainLadderFit)
}
	
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


# class of "MultiChainLadderSummary"
setClass("MultiChainLadderSummary",
	representation(
		Triangles="triangles",
		FullTriangles="triangles",
		S.E.Full="list",
		S.E.Est.Full="list",
		S.E.Proc.Full="list",
		Ultimate="matrix",
		IBNR="matrix",
		S.E.Ult="matrix",
		S.E.Est.Ult="matrix",
		S.E.Proc.Ult="matrix",
		report.summary="list",
		coefficients="list",
		coefCov="list",
		residCov="list",
		rstandard="matrix",
		fitted.values="matrix",		
		residCor="matrix",
		model.summary="matrix",
		portfolio="NullChar")
)	

## Define generic functions 
##

# generic function for Mse calculation
setGeneric("Mse",
           function(ModelFit, FullTriangles, ...)
           standardGeneric("Mse")
)



# generic function for residual covariance
setGeneric("residCov",
               function(object, ...)
               standardGeneric("residCov"))

# generic function for residual correlation
setGeneric("residCor",
               function(object, ...)
               standardGeneric("residCor"))



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
				int=NULL,
				restrict.regMat=NULL,                
				extrap=TRUE ,
            	mse.method="Mack" ,
				model="MCL", ...){
	
	# Convert object to class "triangles". Input data will be validated automatically. 
	Triangles  <- as(Triangles,"triangles")
	
	if (!any(fit.method %in% c("SUR", "OLS")))
		stop("Estimation method must be either SUR or OLS!\n")
		
	if (!any(mse.method %in% c("Mack", "Independence")))
		stop("Mse estimation method is not valid!\n")
	
	
	if (!(model %in% c("MCL","GMCL")))
		stop("model must be either MCL or GMCL!\n")	
	if (model=="GMCL" && mse.method=="Independence")
		warnings("Mse estimation under independence assumption is not available for GMCL.\n
		The Mack method is used automatically!\n",call.=FALSE)
		
	n <- dim(Triangles)[3]
	
	if (!is.null(int) && max(int) > n-1)
		warning("Incorrect intercepts specified!\n",call.=FALSE)
	
	if (model=="MCL"){ 

		# the input triangles need to have at least 4 columns if extrap==TRUE
		if (extrap && n<4) 
			stop("Triangles need to have at least 4 columns for extrapolation!\n")

		# if the last period has enough data to fit regression, set extrap=FALSE 
		if ((sum(!is.na(Triangles[[1]][,n])) > 1) && extrap) {
			warning("Trapezoids do not need exptrapolation.\n 			
			The value of extrap is changed to FALSE.\n", call.=FALSE)
			
			extrap=FALSE
		}
	}	
	
	# call .FitGMCL or .FitMCL to fit regressions	
	
	if (model == "GMCL"){ 
		models  <- .FitGMCL(Triangles=Triangles,
					fit.method=fit.method,
					delta=delta,
					int=int,
					restrict.regMat=restrict.regMat,...)					
	}
		
	if (model == "MCL") {		
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
			coefficients=models@coefficients ,
			coefCov=models@coefCov,
			residCov=models@residCov,
			fit.method=models@fit.method,
			delta=models@delta,
			mse.ay=mse.models@mse.ay,
			mse.ay.est=mse.models@mse.ay.est,
			mse.ay.proc=mse.models@mse.ay.proc,
			mse.total=mse.models@mse.total,
			mse.total.est=mse.models@mse.total.est,
			mse.total.proc=mse.models@mse.total.proc,
			FullTriangles=mse.models@FullTriangles,
			model=model,
			int=models@int,
			restrict.regMat=models@restrict.regMat )   
			
	return(output)
}


# a two-part multivariate chain ladder method
# split the triangles into 2 parts- fit MCL/GMCL on the first part 
# and SCL on the second part
# return the union of the two models
MultiChainLadder2 <- function(Triangles, mse.method = "Mack", last = 3, 
                      type = c("MCL", "MCL+int", "GMCL-int", "GMCL"), ...){
  type <- match.arg(type)
  Triangles <- as(Triangles, "triangles")
  m <- ncol(Triangles[[1]])
  first <- m - last
  # split the data into two parts
  T1 <- Triangles[, 1:first]
  T2 <- Triangles[, first:m]
  
  if (type == "MCL") {               # the MCL model
    # fit SUR MCL on the first part
    f1 <- MultiChainLadder(T1, extrap = FALSE, ...)
  } else if (type == "MCL+int") {    # the MCL plus intercept 
    p <- length(Triangles)
    dm <- matrix(1:(p * (p + 1)), p, p + 1, byrow = TRUE)
    dm2 <- dm[, -1]
    dm2 <- diag(diag(dm2), nrow = p)
    dm <- cbind(dm[, 1], dm2) 
    pp <- t(dm)[t(dm) > 0]
    coefr <- matrix(0, p * (p + 1), 2 * p)
    pos <-  cbind(pp, 1:(2 * p))
    coefr[pos] <- 1         #coefficient restriction matrix
    restrict.regMat <- c(rep(list(coefr), first), rep(list(NULL), last))
    
    # fit SUR GMCL on the first part
    f1 <- MultiChainLadder(T1, int = 1:(first - 1), model = "GMCL",
                          restrict.regMat = restrict.regMat, ...)
    
  } else if (type == "GMCL-int") {    # the GMCL without intercepts 
    f1 <- MultiChainLadder(T1, model = "GMCL",  ...)
    
  } else if (type == "GMCL") {        # the full GMCL model 
    f1 <- MultiChainLadder(T1, int = 1:(first - 1), model = "GMCL", ...)
  }
  
  # fit separate chain ladder on the second part
  f2 <- MultiChainLadder(T2, fit.method = "OLS")
  # join the two models
  ff <- Join2Fits(f1, f2)
  ffT  <-  predict(ff) 
  # compute mse
  mse  <-  Mse(ff, ffT, mse.method)
  # create a new MultiChainLadder object
  fit <- JoinFitMse(ff, mse)
  return(fit)
}

# fit the GMCL model 
.FitGMCL <- function(Triangles,
				fit.method="SUR",
				delta=1,
				int=NULL,
				restrict.regMat=NULL, ...){

  
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]
	d <- delta/2
	 
	myModel <- vector("list",n-1)  #this is a list with all the fitted regressions
	system <- lapply(1:p, function(z) as.formula(paste("y[[",z,"]]~-1+x[[",z,"]]",sep="")))
	
	for (i in 1:(n-1)){   					
		da <- Triangles[1:(m-i),i:(i+1)]
		# x0 and y0 are not weighted
		y0 <- da[,2]
		x0 <- cbind(1,cbind2(da[,1]))			
		# weighted values to be used in systemfit
		y <- lapply(1:p, function(z) y0[[z]]*(x0[,z+1]^(-d)) )
		if (i%in%int) x <- lapply(1:p, function(z) x0*(x0[,z+1]^(-d)) ) else
					  x <- lapply(1:p, function(z) x0[,-1]*(x0[,z+1]^(-d)) )										
		myModel[[i]] <- systemfit(system,fit.method,
								restrict.regMat=restrict.regMat[[i]],...)  
	}
		
	# paramters returned by systemfit	
	coefficients <- lapply(1:length(myModel), function(x) 
							matrix(coef(myModel[[x]]),p,byrow=TRUE))
												
	coefCov <- lapply(myModel, "[[", "coefCov")
	residCov <- lapply(myModel, "[[", "residCov")
						 
	# Transform the coefficients to a form with intercepts  
	coefficients <- .coef(coefficients, int, p)	
	# create an object of class "GMCLFit" 
	output <- new("GMCLFit",
			Triangles=Triangles,
			models=myModel,
			coefficients=coefficients,
			coefCov=coefCov,
			residCov=residCov,
			fit.method=fit.method,
			delta=delta ,
			int=int,
			restrict.regMat=restrict.regMat )			
	return(output)
}

			
					
# fit the MCL model 
.FitMCL <- function(Triangles,
				fit.method="SUR",
				delta=1,
				extrap=TRUE,...)
	{
  
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]
	d <- delta/2
	
	myModel <- vector("list",n-1)
	system <- lapply(1:p, function(z) as.formula(paste("y[[",z,"]]~-1+x[[",z,"]]",sep="")))

	for (i in 1:(n-1)){
		da <- Triangles[1:(m-i),i:(i+1)]
		da <- lapply(1:length(da), function(x) sweep(da[[x]],1,da[[x]][,1]^d,"/"))
		da <- as(da,"triangles")
		y <- da[,2]
		x <- da[,1]													
		if (!(i==n-1 && extrap )) myModel[[i]] <- systemfit(system,fit.method,...)
	}				
	
	if (extrap) {
		coef <- unlist(Triangles[1,n])/unlist(Triangles[1,n-1])
		names(coef) <- names(myModel[[n-2]][["coefficients"]])
			
		r2 <- myModel[[n-3]]$residCov
		r1 <- myModel[[n-2]]$residCov
		r0 <- abs(r1^2/r2)
		residcov <- as.matrix(pmin(abs(r2),abs(r1),replace(r0,is.na(r0),0)))
		dimnames(residcov)<-dimnames(myModel[[n-2]][["residCov"]])
			
		# extrapolate coefCov? should the off-diagonal components be set as zero?
		x <- unlist(Triangles[1,n-1])^d
		v <- solve(diag(x,nrow=p)%*%solve(residcov)%*%diag(x,nrow=p))
		coefcov <- diag(diag(v),nrow=p)
		dimnames(coefcov)<-dimnames(myModel[[n-2]][["coefCov"]])
			
		myModel[[n-1]]<-list(coefficients=coef,coefCov=coefcov,residCov=residcov)
	}
			
	coefficients <- lapply(myModel,"[[","coefficients")
	coefCov <- lapply(myModel, "[[", "coefCov")
	residCov <- lapply(myModel, "[[", "residCov")
		
	# replace off-diagonal elements of residCov as 0
	if (fit.method == "OLS") residCov <- lapply(1:(n-1),function(x) 
								diag(diag(residCov[[x]]),nrow=p))				
	# create an object of class "MCLFit"
	output <- new("MCLFit",
			Triangles=Triangles,
			models=myModel,
			coefficients=coefficients,
			coefCov=coefCov,
			residCov=residCov,
			fit.method=fit.method,
			delta=delta )			
	return(output)
	
}
 


# method to predict the full triangles for "GMCLFit" object
# the augmented procedure is used
setMethod("predict", signature="GMCLFit",
	function(object,...){
		Triangles <- object@Triangles
		# augment parameters, unique to GMCL
		B <- .B.aug(object)
		p <- dim(Triangles)[1]
		m <- dim(Triangles)[2] 
		n <- dim(Triangles)[3]
		FullTriangles <- Triangles
    		for (i in 1:(n-1)){
    		x <- FullTriangles[(m-i+1):m,i]
    		# add a column of 1's    		
    		x.a <- t(cbind(1,cbind2(x)))
        	y <- (B[[i]] %*% x.a)[-1,,drop=FALSE]
			FullTriangles[(m-i+1):m,(i+1)] <- split(y,1:nrow(y))
		}
		return(FullTriangles)
	}
)

# method to predict the full triangles for "MCLFit" object
setMethod("predict", signature="MCLFit",
	function(object,...){
	Triangles <- object@Triangles
	B <- object@coefficients
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]
	FullTriangles <- Triangles
    	for (i in 1:(n-1)){
    		x <- t(cbind2(FullTriangles[(m-i+1):m,i]))
			y <- diag(B[[i]],nrow=p)%*%x
			FullTriangles[(m-i+1):m,(i+1)] <- split(y,1:nrow(y))		
	}
	return(FullTriangles)
	}
)


# method to calculation mse for "GMCL" 
# augmented approach is used
setMethod("Mse",signature(ModelFit="GMCLFit",
						FullTriangles="triangles"),
	function(ModelFit, FullTriangles, ...){

	Triangles <- ModelFit@Triangles
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]
   	d <- ModelFit@delta/2
	I <- diag(rep(1,p+1))
	
	# augment coefficients, residual covariance matrices 
	# and covariance matrices for estimated coefficients
	B <- .B.aug(ModelFit)
	Bcov <- .Bcov.aug(ModelFit)
	ecov <- .ecov.aug(ModelFit)
	
	mse.ay <- mse.ay.est <- mse.ay.proc <- matrix(0,m*p,n*p)
	mse.total <- mse.total.est <- mse.total.proc <- matrix(0,p,p*n)
	
	# recursive calcualtion of mse for single accident years

	for (k in 1:(n-1)) {
		for (i in m:(m+1-k)){
			a1 <- (p*(i-1)+1):(p*i)   	# old indexes
			b1 <- (p*(k-1)+1):(p*k)
			a2 <- (p*(i-1)+1):(p*i)		# new indexes
			b2 <- (p*k+1):(p*(k+1))
			
			yhat <- rbind2(FullTriangles[i,k])
			# augmented by adding an one in front
			yhat.a <- c(1,yhat)   
			
			# process variance
			proc.old <- mse.ay.proc[a1,b1]

			# recursive calculation of augmented mse
			proc.new.a <- B[[k]]%*%.add.zero(proc.old)%*%t(B[[k]])+
					diag((yhat.a)^d,nrow=p+1)%*%ecov[[k]]%*%diag((yhat.a)^d,nrow=p+1)

			# predicted mse on non-augmented vectors
			mse.ay.proc[a2,b2] <- .rm.zero(proc.new.a)

			# estimation variance	
			est.old <- mse.ay.est[a1,b1]

			# recursive calculation of augmented mse		
			est.new.a <- B[[k]]%*% .add.zero(est.old) %*%t(B[[k]])+
						kronecker(t(yhat.a),I)%*%Bcov[[k]]%*%kronecker(yhat.a,I)

 			# predicted mse on non-augmented vectors
			mse.ay.est[a2,b2] <- .rm.zero(est.new.a)

 			# combined
			mse.ay[a2,b2] <- mse.ay.proc[a2,b2]+mse.ay.est[a2,b2]					
		}
	}
		
	# recursive calcualtion of mse for aggregated accident years
	for (k in 1:(n-1))
		{
		b1 <- (p*(k-1)+1):(p*k)   #old index
		b2 <- (p*k+1):(p*(k+1))   #new index

		yhat <- FullTriangles[(m+1-k):m,k]
		
		yhat.a <- cbind2(1,cbind2(yhat))
		# process variance
		proc.sum.a <- lapply(1:nrow(yhat.a), function(x){							
							dy <- diag(yhat.a[x,],nrow=p+1)^d 
							 dy %*% ecov[[k]] %*% dy})
		proc.sum.a <- Reduce("+",proc.sum.a)
		
		# augmented total process variance
		mse.total.proc.a <- B[[k]]%*%.add.zero(mse.total.proc[,b1])%*%t(B[[k]])+proc.sum.a
		mse.total.proc[,b2] <- .rm.zero(mse.total.proc.a)

		# estimation variance 
		
		yhat.sum.a <- apply(cbind2(1,cbind2(yhat)),2,sum)

		mse.total.est.a <- B[[k]]%*%.add.zero(mse.total.est[,b1])%*%t(B[[k]])+
					kronecker(t(yhat.sum.a),I)%*%Bcov[[k]]%*%kronecker(yhat.sum.a,I)
		mse.total.est[,b2] <- .rm.zero(mse.total.est.a) 
	
		# combined					
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
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]
   	d <- ModelFit@delta/2
	B <- ModelFit@coefficients
	Bcov <- ModelFit@coefCov
	ecov <- ModelFit@residCov

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

			yhat <- as.vector(rbind2(FullTriangles[i,k]))
			#process variance
			mse.ay.proc[a2,b2] <- diag(yhat^d,nrow=p)%*%ecov[[k]]%*%diag(yhat^d,nrow=p)+
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
		yhat <- cbind2(FullTriangles[(m+1-k):m,k])
		
		proc.sum <- lapply(1:nrow(yhat), function(x){							
							dy <- diag(yhat[x,],nrow=p)^d 
							 dy %*% ecov[[k]] %*% dy
							})
		proc.sum <- Reduce("+",proc.sum)
		
		yhat.sum <- apply(yhat,2,sum)
		
		mse.total.proc[,b2] <- proc.sum+B[[k]]%*%t(B[[k]])*mse.total.proc[,b1]

		#estimation variance
		if (mse.method %in% "Mack")
			{
			mse.total.est[,b2] <- Bcov[[k]]*(yhat.sum %*%t(yhat.sum))+
									(B[[k]]%*%t(B[[k]]))*mse.total.est[,b1]
			}
		if (mse.method %in% "Independence")
			{
			mse.total.est[,b2] <- Bcov[[k]]*(yhat.sum %*%t(yhat.sum))+
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
# portfolio can be used to calculate the sum of two triangles. 
# If NULL, then all triangles will be summed.

setMethod("summary", signature(object="MultiChainLadder"),
	function(object,portfolio=NULL,...){

	Triangles <- object@Triangles
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]

	if (p==1) 
		portfolio=NULL else 
		if (is.null(portfolio)) 
				portfolio <- 1:p else
				portfolio <- as.numeric(unlist(strsplit(portfolio,"\\+",perl=TRUE)))

	# ultimate statistics
	Ultimate <- .ultimate(object,portfolio=portfolio)
	Latest	<- .latest(object,portfolio=portfolio)
	IBNR <- Ultimate-Latest
	Dev.To.Date <-  Latest/Ultimate
	S.E.Ult <- .se.ult(object,portfolio=portfolio,type="mse")
	S.E.Est.Ult <- .se.ult(object,portfolio=portfolio,type="est")
	S.E.Proc.Ult <- .se.ult(object,portfolio=portfolio,type="proc")
	CV <- S.E.Ult/IBNR
	CV[is.na(CV)] <- 0
	
	# full se statistics
	S.E.Full <- .se.all(object,portfolio=portfolio,type="mse")
	S.E.Est.Full <- .se.all(object,portfolio=portfolio,type="est")
	S.E.Proc.Full <- .se.all(object,portfolio=portfolio,type="proc")
	
	
	# standardized residuals
	resid.st <- rstandard(object)
	dev <- rep(1:length(resid.st),sapply(resid.st,nrow))
	resid.st <- as.matrix(cbind(do.call("rbind",resid.st),dev))
	dimnames(resid.st)[[2]] <- c(as.character(1:p),"dev")
	
	# fitted values
	fitted.values <- fitted(object)
	fitted.values <- as.matrix(cbind(do.call("rbind",fitted.values),dev))
	dimnames(fitted.values)[[2]] <- c(as.character(1:p),"dev")
	
	# model summary
	model.sum <- .model.summary(object)
	dev <- rep(1:length(model.sum),sapply(model.sum,nrow))
	model.sum <- as.matrix(cbind(do.call("rbind",model.sum),dev=dev))
	
	# residual correlation for multiple triangles
	if (p>1) {
		resid.cor <- residCor(object)
		
		resid.cor <- as.matrix(cbind(do.call("rbind",resid.cor),
							rep(1:(n-1),rep(nrow(resid.cor[[1]]),n-1))))
		dimnames(resid.cor)[[2]] <-c("residCor","dev")
	} else {
		resid.cor <- matrix(0,0,0) 
		}
	
	portfolio <- if(!is.null(portfolio)) paste(portfolio,collapse="+")
	
	n2 <- ncol(Ultimate)
	output <- lapply(1:n2, function(x) {
						output <- data.frame(Latest=Latest[,x], 
									Dev.To.Date=round(Dev.To.Date[,x],4), 
									Ultimate=Ultimate[,x], 
									IBNR=IBNR[,x], 
									S.E=round(S.E.Ult[,x],2), 
									CV=round(CV[,x],4))
						rownames(output) <- c(as.character(1:m),"Total")
						return(output) })
	if (p>1) {
		nm <-  paste("Summary Statistics for Triangle", 
						c(as.character(1:(n2-1)), portfolio))} else{
		nm <- "Summary Statistics for Input Triangle"}
	names(output) <- nm
	
	
	output2=new("MultiChainLadderSummary",
					Triangles=Triangles,
					FullTriangles=object@FullTriangles,
					S.E.Full=S.E.Full,
					S.E.Est.Full=S.E.Est.Full,	
					S.E.Proc.Full=S.E.Proc.Full,
					Ultimate=Ultimate,
					IBNR=IBNR,
					S.E.Ult=S.E.Ult,
					S.E.Est.Ult=S.E.Est.Ult,
					S.E.Proc.Ult=S.E.Proc.Ult,
					report.summary=output,
					coefficients=object@coefficients,
					coefCov=object@coefCov,
					residCov=object@residCov,
					rstandard=resid.st,
					fitted.values=fitted.values,
					residCor=resid.cor,
					model.summary=model.sum,
					portfolio=portfolio )
	return(output2)				
	
	}
)



setMethod("show",signature(object = "MultiChainLadderSummary"),
	function(object){
		s<-object@report.summary
		s <- lapply(s,function(x) format(x,big.mark = ",", digits = 3))
		print(s)  # a list  
	}
)




setMethod("show",
    signature(object = "MultiChainLadder"),
    function (object) 
    {
    s <- summary(object)
	show(s)
	}
    
)


# This function joins two pieces of one object of "triangles" together. The input
# triangles should be result of "[" for class "triangles" designed to fit
# different models for different periods. This function is used internally by "Join2Fits".

.Join2Triangles <- function(triangles1,triangles2){
	# triangles1 must come from the first several developments 
	if (dim(triangles1)[2] < dim(triangles2)[2])
		stop("The first object must have more rows!\n")

	m1=dim(triangles1)[2]
	n1=dim(triangles1)[3]
	m2=dim(triangles2)[2]
	n2=dim(triangles2)[3]
	
	.triangles1 <- triangles1[,n1]
	.triangles2 <- triangles2[,1]
	
	# check to see if the two triangles can be joined 
	
	if (!all.equal(rbind2(.triangles1), rbind2(.triangles2)))
		stop("The two triangles can not be joined!\n 
		Make sure the last columns of the first triangles agree with the 
		first columns of the second triangels!\n")
		
	t2=triangles2[,2:n2]
	na=matrix(NA,m1-m2+1,n2-1)
	t2=lapply(t2,rbind,na)	
	Triangles=lapply(1:length(triangles1), function(x) cbind(triangles1[[x]],t2[[x]]))

	Triangles=as(Triangles,"triangles")
	return(Triangles)
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
			coefficients=c(object1@coefficients,object2@coefficients) ,
			coefCov=c(object1@coefCov,object2@coefCov),
			residCov=c(object1@residCov,object2@residCov),
			fit.method=c(object1@fit.method,object2@fit.method),
			delta=object1@delta)
	}

	# To join GMCL with MCL, the MCL paramters are transformed to GMCL w/o 
	# intercept format and set int=object1@int.
	
	if (any(model=="GMCL") && any(model=="MCL")) {
		# assume the first is GMCL and the latter is MCL
		coef2 <- lapply(object2@coefficients,diag,length(object1@Triangles))
		coef2 <- .coef(coef2,int=NULL,length(object1@Triangles))
		output <- new("GMCLFit", 
			Triangles=.Join2Triangles(object1@Triangles,object2@Triangles),
			models=c(object1@models,object2@models), 
			coefficients=c(object1@coefficients,coef2) , 
			coefCov=c(object1@coefCov,lapply(object2@coefCov,.M2G.coefCov)),
			residCov=c(object1@residCov,object2@residCov),
			fit.method=c(object1@fit.method,object2@fit.method),
			delta=object1@delta,
			int=object1@int,
			restrict.regMat=object1@restrict.regMat)
	}

	if (all(model=="GMCL")) {

		output <- new("GMCLFit", 
			Triangles=.Join2Triangles(object1@Triangles,object2@Triangles),
			models=c(object1@models,object2@models),
			coefficients=c(object1@coefficients,object2@coefficients) ,
			Bcov=c(object1@Bcov,object2@Bcov),
			residCov=c(object1@residCov,object2@residCov),
			fit.method=c(object1@fit.method,object2@fit.method),
			delta=object1@delta,
			int=c(object1@int,object2@int),
			restrict.regMat=c(object1@restrict.regMat,object2@restrict.regMat))
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
			coefficients=models@coefficients ,
			coefCov=models@coefCov,
			residCov=models@residCov,
			fit.method=models@fit.method,
			delta=models@delta,
			mse.ay=mse.models@mse.ay,
			mse.ay.est=mse.models@mse.ay.est,
			mse.ay.proc=mse.models@mse.ay.proc,
			mse.total=mse.models@mse.total,
			mse.total.est=mse.models@mse.total.est,
			mse.total.proc=mse.models@mse.total.proc,
			FullTriangles=mse.models@FullTriangles ,
			model=model,
			int=models@int,
			restrict.regMat=models@restrict.regMat) 
	return(output)

}

# method for "as" to convert list to "tirangles"
setAs("list","triangles", function(from) {
							from2 <- lapply(from, as.matrix)
							new("triangles",from2)
							} )



# method to extract certain columns 
# and decide whether to drop rows with all NA's according to drop
setMethod("[", signature(x = "triangles", i = "missing", j = "numeric",
			 drop = "logical"),
	function (x, i, j, ..., drop) {
		output <- lapply(x, "[", ,j, drop=FALSE)
		if (drop) {
			na.rows <- apply(is.na(output[[1]]),1,all)
			output <- lapply(output, "[", !na.rows, ,drop=FALSE)
			}
		as(output,"triangles")
	}
)

# method to extract certain columns 
# and drop rows with all NA's if drop is missing
setMethod("[", signature(x = "triangles", i = "missing", j = "numeric",
			 drop = "missing"),
	function (x, i, j, ..., drop) {
		x[,j,drop=TRUE]
	}
)


# method to extract certain rows 
# and decide whether to drop columns with all NA's
setMethod("[", signature(x = "triangles", i = "numeric", j = "missing",
			 drop = "logical"),
	function (x, i, j, ..., drop=TRUE) {
		output <- lapply(x, "[", i, , drop=FALSE) 
		if (drop) {
			na.cols <- apply(is.na(output[[1]]),2,all)
			output <- lapply(output, "[", , !na.cols, drop=FALSE)
			}
		as(output,"triangles")
	}
)

# method to extract certain rows and 
#  drop columns with all NA's if drop is missing
setMethod("[", signature(x = "triangles", i = "numeric", j = "missing",
			 drop = "missing"),
	function (x, i, j, ..., drop) {
		x[i,,drop=TRUE]
	}
)

# method to extract certain rows and columns 
setMethod("[", signature(x = "triangles", i = "numeric", j = "numeric",
			 drop = "missing"),
	function (x, i, j, ..., drop) {
		output <- lapply(x, "[", i, j,drop=FALSE)
		as(output,"triangles")
	}
)

# replacement method
setMethod("[<-", signature(x = "triangles", i = "numeric", j = "numeric",
			 value = "list"),
	function (x, i, j, ..., value) {
		for (y in 1:length(x)) {
				x[[y]][i,j] <- value[[y]]
			}
		return(x)
	}
)

# extract the dimenstions of "triangles"
setMethod(dim,signature(x="triangles"),
	function(x)
	{
	p <- length(x)
	m <- nrow(x[[1]])
	n <- ncol(x[[1]])	
	c(p,m,n)						
	}
)

	
## rbind all triangles
setMethod(rbind2,signature(x="triangles",y="missing"),
	function(x,y)
	{
	do.call("rbind",x)			
	}
)	

# cbind all triangles
setMethod(cbind2,signature(x="triangles",y="missing"),
	function(x,y)
	{
	do.call("cbind",x)			
	}
)	

setMethod("$",
    signature(x = "MultiChainLadder"),
    function (x, name) 
    {
        slot(x,name)
    }
)

setMethod("names",
    signature(x = "MultiChainLadder"),
    function (x) 
    {
        return(slotNames(x))
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

setMethod("$",
    signature(x = "MultiChainLadderSummary"),
    function (x, name) 
    {
        slot(x,name)
    }
)

setMethod("names",
    signature(x = "MultiChainLadderSummary"),
    function (x) 
    {
        return(slotNames(x))
    }
)

setMethod("[[",
    signature(x = "MultiChainLadderSummary",i="numeric",j="missing"),
    function (x, i, j, ...) 
    {
	  output <- lapply(i, function(y) slot(x,names(x)[y]))
      names(output) <- names(x)[i]
	  return(output)
    }
)

setMethod("[[",
    signature(x = "MultiChainLadderSummary",i="character",j="missing"),
    function (x, i, j, ...) 
    {
	  output <- lapply(1:length(i), function(y) slot(x,i[y]))
      names(output) <- i
	  return(output)
    }
)




setMethod("coef",
	signature(object = "MultiChainLadder"),
    function (object,...) 
    {
	return(object@coefficients)
    }
)

# variance-covariance matrix as returned by systemfit.
setMethod("vcov",
	signature(object = "MultiChainLadder"),
    function (object,...) 
    {
	return(object@coefCov)
    }
)

# method to extract residual covariance
setMethod("residCov",
    signature(object = "MultiChainLadder"),
    function (object, ...) 
    {
	return(object@residCov)
    }
)

# method to extract residual correlation
setMethod("residCor",
    signature(object = "MultiChainLadder"),
    function (object, ...) 
    {
	rv <- residCov(object)
	return(lapply(rv, .cor))
    }
)

# extract correlations and put into a column vector
.cor <- function(object){
	p1 <- row(object)[upper.tri(object)]
	p2 <- col(object)[upper.tri(object)]	
	p <- cbind(p1,p2)
	rho <- matrix(0,nrow(p),1)
	for (i in 1:nrow(p)){
		a <- p[i,1]
		b <- p[i,2]
		rho[i,] <- object[a,b]/sqrt(object[a,a]*object[b,b])	}
	row.names(rho) <- paste("(",p1,",", p2,")",sep="")
	
	return(rho)	
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
	
	# if extrapolation is used, just set the residual at the last period
	# to be zero to be consistent with other definitions
	if (cl[K]!= "systemfit") {
		rl <- as.data.frame(matrix(0,1,p))
		names(rl) <- names(r[[n]])
		r <- c(r,list(rl))
		}
	return(r)
    }
)


setMethod("resid",
    signature(object = "MultiChainLadder"),
    function (object, ...) 
    {
	return(residuals(object))
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
	model <- object@model
	Triangles <- object@Triangles
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]
	fitted=vector("list",n-1)

	if (model=="GMCL") {
			B <- .B.aug(object)
			for (i in 1:(n-1)){
				x <- Triangles[1:(m-i),i]
				x.a <- t(cbind(1,cbind2(x))) 
				y<- (B[[i]] %*% x.a)[-1,,drop=FALSE]
				fitted[[i]] <- t(y)
			}
	}
	if (model=="MCL") {
			B <- coef(object)
    			for (i in 1:(n-1)){
        			x <- sapply(Triangles, "[", 1:(m-i),i)  
      			fitted[[i]] <- x%*%diag(B[[i]],nrow=p)
			}
	}

	return(fitted)
    }
)





setMethod("plot",
    signature(x = "MultiChainLadder",y="missing"),
    function (x, y, which.plot=1:4, 
				which.triangle=NULL, 
				main=NULL,  
				portfolio=NULL,
				lowess=TRUE, 
				legend.cex=0.75,...) 
    {
    	
    p <- length(x@Triangles)
    if (is.null(which.triangle)) which.triangle <- if (p>1) 1:(p+1) else 1	
    if (!is.numeric(which.triangle) || 
		any(which.triangle < 0) || 
		any(which.triangle > ifelse(p>1,p+1,1)))
    	stop("The value of which.triangle is not valid!\n")
    	
    if (!is.numeric(which.plot) || 
		any(which.plot < 1) || 
		any(which.plot > 5)) 		
		stop("The value of which.plot must be in 1:5!\n")
    
    lw <-  length(which.plot)	
	if (!is.null(main) && (!is.list(main) || length(main)!=lw))
		stop("main must be a list of equal length with which.plot!\n")	
	
	.summary <- summary(x,portfolio=portfolio)

	if (any(which.plot==1)){
		.myResult <-  .summary@report.summary 
		n <-  nrow(.myResult[[1]])
		for (i in which.triangle){ 
			mp <- match(1, which.plot)			
			if (!is.null(main)) main2 <- main[[mp]][i] else 
				main2 <- if (i <=p) paste("Barplot for Triangle",i) else 
						"Portfolio"
				
        		plotdata <- t(as.matrix(.myResult[[i]][-n,c("Latest","IBNR")]))       		
			ymax <- max(apply(.myResult[[i]][-n,c("Ultimate", "S.E")],1,sum))
				
			bp <- barplot(plotdata,
                      		names.arg=rownames(.myResult),
                      		main=main2,
                 			xlab="Origin period",
                 			ylab="Value",
                      		ylim=c(0, 1.25*ymax),...)
						
			legend("topleft",c("Latest","Forecast"),
						fill=c("#4D4D4D", "#E6E6E6"), #gray.colors(2),
						inset=c(0.1,0.1),
						cex=legend.cex)

       		.errbar(x=bp, 
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
		n <- dim(.Triangles)[3]	
		if (p>1) {
			.Triangles[[p+1]] <- Reduce("+",.Triangles)  
			.FullTriangles[[p+1]] <- Reduce("+",.FullTriangles)  
		}
			
		for (i in which.triangle){
			mp <- match(2, which.plot)			
			if (!is.null(main)) main2 <- main[[mp]][i] else {
				main2 <- if (i <=p) paste("Development Pattern for Triangle",i) 
					else "Portfolio"
					 }
				
			matplot(t(.FullTriangles[[i]]), 
					type="l",
                			main=main2,
                			xlab="Development period",
                			ylab="Amount",
					col=1:10,...)
        			
			text((n + runif(10,-1,0)),.FullTriangles[[i]][,n],as.character(1:10),col=1:10)
	
		}
	}

	if (any(which.plot==3) || any(which.plot==4)){
		r <- .summary@rstandard
		fitted <-  .summary@fitted.values
		which.triangle <- which.triangle[which(which.triangle!=(p+1))]  
		# can not plot portfolio residuals
			
		if (any(which.plot==3)){ 
			for (i in which.triangle){
			mp <- match(3, which.plot)			
			if (!is.null(main)) main2 <- main[[mp]][i] else  
					main2 <- paste("Residual Plot for Triangle", i)

				plot(fitted[,i],r[,i],
						main=main2,
						ylab="Standardised residuals", 
						xlab="Fitted",
						cex=0.75,...)
				if (lowess) lines(lowess(fitted[,i], r[,i]), col="red")
        			abline(h=0, col="grey")
			}
		}			
			
		if (any(which.plot==4)){ 
			for (i in which.triangle){
				mp <- match(4, which.plot)		
				if (!is.null(main)) main2 <- main[[mp]][i] else  
					main2 <- paste("QQ-Plot for Triangle", i)

				qqnorm(r[,i],main=main2,cex=0.75,...)
        			abline(0,1)
			}
		}		
	}
		
	if (any(which.plot==5)) {
		.Triangles <- .summary@Triangles
		.FullTriangles <- .summary@FullTriangles
	
		if (p>1) {
			.Triangles[[p+1]] <- Reduce("+",.Triangles)  
			.FullTriangles[[p+1]] <- Reduce("+",.FullTriangles)  
		}
			
		.S.E.Full <- .summary@S.E.Full
		n <- nrow(.S.E.Full[[1]])
		long <-  expand.grid(origin=1:nrow(.Triangles[[1]]),
						dev=1:ncol(.Triangles[[1]]))
			
		for (i in which.triangle){
			mp <- match(5, which.plot)			
			if (!is.null(main)) main2 <- main[[mp]][i] else {
				main2 <- if (i <=p) paste("Development Pattern for Triangle",i) else 
						"Portfolio"
			}
					 
			long$value <- as.vector(.FullTriangles[[i]])
			long$valuePlusS.E <-  long$value + as.vector(.S.E.Full[[i]][-n,])
			long$valueMinusS.E <-  long$value - as.vector(.S.E.Full[[i]][-n,])
				
				
			xy <- xyplot(valuePlusS.E + valueMinusS.E + value ~ dev |factor(origin), 
        				 	data=long[!is.na(long$value),], 
        				 	t="l", 
        				 	lty=c(3,3,1), 
        				 	as.table=TRUE,
               				main=main2,
               				xlab="Development period",
              				ylab="Amount",
						col=1,
               				key=list(lines=list(lty=c(1,3), col=1),
               					text=list(lab=c("Development", "Mack's S.E.")),
               					space="top", 
               					columns=2))

			print(xy)            		
           }
     	}
	}	
)

# functions to augment parameters from systemfit to the format desired
# the three augmented sets of parameters are called B, Bcov and ecov respectively

# function to add a row and a column of zeros 
.add.zero <- function(object){
	object <- as.matrix(object)
	m <- nrow(object)
	n <- ncol(object)
	object2 <- matrix(0,m+1,n+1)
	object2[2:(m+1),2:(n+1)] <- object
	return(object2)
}

# function to remove the first row and the first column 
.rm.zero <- function(object){
	object <- as.matrix(object)
	m <- nrow(object)
	n <- ncol(object)
	object2 <- object[2:m,2:n]
	return(object2)
}


# function used in .FitGMCL to transform the coefficients
# to matrices with intercepts. 
# if no intercept specified, then pad a columns of zero
.coef <- function(coefficients, int, p){
		n <- length(coefficients)
		coeff <- rep(list(matrix(0,p,p+1)),n)   
		for (i in 1:n){
			if (!i %in% int) cols <- 2:(p+1) else cols <- 1:(p+1)
			coeff[[i]][,cols] <- coefficients[[i]]
		}
		return(coeff)
}

# function to augment coefficients to be used in prediction and mse estimation
# change the coefficent matrix  to (p+1)* (p+1) since a vector (1,0, \cdots 0)' is added
.B.aug <- function(object){
	B <- lapply(object@coefficients, function(x){
							a <-.add.zero(x)[,-1]
							a[1,1] <- 1
							return(a)})
	return(B)
}


# function to augment the coeffient covariance matrix to the format desired
.Bcov.aug <- function(object){
		p <- length(object@Triangles)
		n <- length(object@coefCov)
		Bcov <- rep(list(matrix(0,(p+1)^2, (p+1)^2)),n)
		chngOrder <- as.vector(matrix(1:((p+1)^2),p+1,p+1,byrow=TRUE))
		
		for (i in 1:n){
			
			# positions of the returned covariance matrix in the 
			# augmented matrix, depending on whether there's an intercept
			if (!i %in% object@int) 
				pos <- apply(expand.grid(2:(p+1),1:p*(p+1)),1,sum) else{
				pos <- (p+2):(p+1)^2 
			}
			
			# first make everything consistent with the systemfit format
			Bcov[[i]][pos,pos]<- object@coefCov[[i]]
			# then transform according to the vetorization
			Bcov[[i]] <- Bcov[[i]][chngOrder,chngOrder]	
		}
		return(Bcov)
}

# function to augment the residual covariance matrix to the format desired
# just add a row and a column of zeros
.ecov.aug <- function(object){
		ecov <- lapply(object@residCov, .add.zero)
		return(ecov)
}



# function to transform the coefCov from MCL to GMCL w/o intercept format
.M2G.coefCov <- function(object){
	object <- as.matrix(object)
	p <- nrow(object)
	object2 <- matrix(0,p^2,p^2)
	pos <- 0:(p-1)*p+1:p	
	object2[pos,pos] <- object
	return(object2)
	}

# function to extract model summary information from systemfit
.model.summary <- function(object){
	models <- object@models
	m <- which(sapply(models,class)=="systemfit")
	lapply(m,function(x) summary(models[[x]])$coefficients)
	}
	


## functions to get summary statistics of reserve or se

# last  column, the ultimate values including all ay's
# if total, then sum across all ays
# if portfolio, then sum columns indicated by portfolio
# note that portfolio is either numeric (passed by summary) or NULL	

.ultimate <- function(object,portfolio=NULL){
	Triangles <- object@Triangles
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]
	FullTriangles <- object@FullTriangles	
	ult <- cbind2(FullTriangles[,n])
	ult <- rbind(ult, apply(ult,2,sum))
	if (!is.null(portfolio)) ult <- cbind(ult,apply(ult[,portfolio],1,sum))
	dimnames(ult)[[1]] <- c(as.character(1:m),"Total")
	dimnames(ult)[[2]] <- if(is.null(portfolio)) 
							as.character(1:p) else 
							c(as.character(1:p),paste(portfolio,collapse="+"))
						
	return(ult)
	}

# latest observed values including all ay's starting from lowest ay
.latest<- function(object,portfolio=NULL){
	Triangles <- object@Triangles
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]
	s <- row(Triangles[[1]])+col(Triangles[[1]])==m+1
	s[1:which(s[,n]==1),n] <- TRUE
	lat <- do.call("cbind",lapply(Triangles, "[", s))
	lat <- lat[nrow(lat):1,,drop=FALSE]   #reverse the order
	lat <- rbind(lat, apply(lat,2,sum))
	if (!is.null(portfolio)) lat <- cbind(lat,apply(lat[,portfolio],1,sum))
	dimnames(lat)[[1]] <- c(as.character(1:(nrow(lat)-1)),"Total")
	dimnames(lat)[[2]] <- if(is.null(portfolio)) 
							as.character(1:p) else 
							c(as.character(1:p),paste(portfolio,collapse="+"))
	return(lat)
	}


# function to extract se's by ay and dev year
# output is a list of matrices

.se.all <- function(object,portfolio=NULL,type="mse"){
	Triangles <- object@Triangles
	p <- dim(Triangles)[1]
	m <- dim(Triangles)[2] 
	n <- dim(Triangles)[3]
	.mse <- if (type=="mse") rbind(object@mse.ay,object@mse.total) else 
			if (type=="est") rbind(object@mse.ay.est,object@mse.total.est) else 
			if (type=="proc") rbind(object@mse.ay.proc,object@mse.total.proc)

	r <- nrow(.mse)/p

	mse.ls <- lapply(1:p, function(x) .mse[((1:r-1)*p+x),.index(x,n,p)])
	
	if (!is.null(portfolio)){
			mse2 <- .mse[.index(portfolio,r,p),.index(portfolio,n,p)]
			lp <- length(portfolio)
			mse.p <- matrix(0,r,n)
			for (i in 1:r){
				for (j in 1:n){
					mse.p[i,j] <- sum(mse2[((i-1)*lp+1):(i*lp),((j-1)*lp+1):(j*lp)])
					}
				}
			mse.ls[[p+1]] <- mse.p
		}
		
	
	return(lapply(mse.ls, sqrt))	
	}

# function to get the se for ultimate losses
# call .se.all internally	
.se.ult <- 	function(object,portfolio=NULL,type="mse"){
	se <- .se.all(object=object,
					portfolio=portfolio,
					type=type)
	p <- length(object@Triangles)				
	m=nrow(se[[1]])-1
	n <- ncol(se[[1]])
	se.ult <- lapply(se, "[",,n,drop=FALSE)
	se.ult <- do.call("cbind",se.ult)
	dimnames(se.ult)[[1]] <- c(as.character(1:m),"Total")
	dimnames(se.ult)[[2]] <- if(is.null(portfolio)) 1 else 
							c(as.character(1:p),paste(portfolio,collapse="+"))
	return(se.ult)
}

# this function returns index from 1:(p*n) where the modulus is x
# p is # triangles, n is # columns, x is the x-th triangle
.index <- function(x,n,p){
	a <- 1:(p*n)
	y <- a %% p
	y <- ifelse(y==0,p,y)
	y %in% x
	}
	
