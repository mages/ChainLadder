

###################################################################################
#####Functions to compute the General Multivariate Chain Ladder(GMCL) model #######
###################################################################################



GeneralMultiChainLadder <- function(triangles,fit.method="SUR", delta=1,...){

    ## Check if the dimensions of all triangles are equal
    if(!check.triangles.dimensions(triangles)){
        stop("Please ensure all triangles have the same dimensions.\n")
    }

	p <- length(triangles)
	m <- dim(triangles[[1]])[1]
	n <- dim(triangles[[1]])[2]
	
	# fit regression models
	models <- fit.GeneralMultiChainLadder(triangles=triangles,
						fit.method=fit.method,
						delta=delta,...)
	
	# complete triangles
	fullTriangles <- predict(models)	 	
	
	# calculate mse
	mse <- mse.GeneralMultiChainLadder(models=models,
						fullTriangles=fullTriangles)
	
	output <- c(models,mse)
	
	class(output) <- c("GeneralMultiChainLadder","list")    
	return(output)
}

fit.GeneralMultiChainLadder <- function(triangles,
						fit.method="SUR",
						delta=1, ...)
	{
	p <- length(triangles)
	m <- dim(triangles[[1]])[1]
	n <- dim(triangles[[1]])[2]
	myModel <- vector("list",n-1)  #########this is a list with all the fitted regressions
	B <- Bcov <- ecov <- vector("list",n-1)
	chngOrder <- as.vector(matrix(1:(p^2),p,p,byrow=TRUE))    # Change the order due to the vectorization  
		for (i in 1:(n-1)){   			
			y <- x <- system <- vector("list",p)
			for (j in 1:p){
				y[[j]] <- triangles[[j]][1:(m-i),i+1]*(triangles[[j]][1:(m-i),i])^(-delta/2)   
				x[[j]] <- lapply(1:p,function(x) triangles[[x]][1:(m-i),i]*(triangles[[j]][1:(m-i),i])^(-delta/2)) 
				
				system[[j]] <- as.formula(
							paste("y[[",j,"]]~-1+",   
								paste("x[[",j,"]][[",1:p,"]]",sep="",collapse="+"),sep="")) 
			}
			
			myModel[[i]] <- systemfit(system,fit.method,...)  
			B[[i]] <-  matrix(coef(myModel[[i]]),p,p,byrow=TRUE)  
			Bcov[[i]] <- vcov(myModel[[i]])[chngOrder,chngOrder] 
			ecov[[i]] <- myModel[[i]]$residCov  
		}
	output <- list()
	output[["triangles"]] <- triangles
	output[["models"]] <- myModel
	output[["B"]] <- B
	output[["Bcov"]] <- Bcov
	output[["ecov"]] <- ecov
	output[["fit.method"]] <- fit.method
	output[["delta"]] <- delta
	class(output) <- "GeneralMultiChainLadderFit"
	return(output)
}

predict.GeneralMultiChainLadderFit <- function(object,...)
	{
	triangles <- object$triangles
	B <- object$B
	p <- length(triangles)
	m <- dim(triangles[[1]])[1]
	n <- dim(triangles[[1]])[2]
	fullTriangles <- triangles
    	for (i in 1:(n-1)){
        	x <-  sapply(1:p, function(x) fullTriangles[[x]][(m-i+1):m,i])  
        	x <- matrix(as.vector(x),p,i,byrow=TRUE)
			y <- B[[i]]%*%x
			for (j in 1:p){
				fullTriangles[[j]][(m-i+1):m,(i+1)] <- y[j,]
			}
		}
	return(fullTriangles)
	}


#####################Functions to compute the MSE in the  gmcl model#####################

mse.GeneralMultiChainLadder <- function(models, fullTriangles){

	triangles <- models$triangles
	p <- length(fullTriangles)
	n <- ncol(fullTriangles[[1]])
	m <- nrow(fullTriangles[[1]])
   	nAdd <- sum(!is.na(triangles[[1]][,n]))-1   	#####number of rows in addition to the traditional triangle
	Ip <- diag(rep(1,p))
	B <- models$B
	Bcov <- models$Bcov
	ecov <- models$ecov
	delta <- models$delta
	
	mse.ay <- mse.ay.est <- mse.ay.proc <- matrix(0,m*p,n*p)
	mse.total <- mse.total.est <- mse.total.proc <- matrix(0,p,p*n)
	
	# recursive calcualtion of mse for single accident years
	for ( i in (2+nAdd):m)
		{
		for (k in (n+nAdd+1-i):(n-1))
			{
			yhat <- sapply(1:p,function(x) fullTriangles[[x]][i,k])
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
			yhat <- sapply(1:p,function(x) fullTriangles[[x]][i,k])
			
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
	return(list(mse.ay=mse.ay,
				mse.ay.est=mse.ay.est,
				mse.ay.proc=mse.ay.proc,
				mse.total=mse.total,
				mse.total.est=mse.total.est,
				mse.total.proc=mse.total.proc,
				fullTriangles=fullTriangles))
	}


##### summarize the gmcl results

summary.GeneralMultiChainLadder <- function(object,agg=NULL,...){
	
	p <- length(object$triangles)
	m <- dim(object$triangles[[1]])[1]
	n <- dim(object$triangles[[1]])[2]
	nAdd <- sum(!is.na(object$triangles[[1]][,n]))-1

	if (is.null(agg)) agg <- 1:p else agg <- as.numeric(unlist(strsplit(agg,"\\+",perl=TRUE)))
	
	Latest <- lapply(1:p, function(x) {
		c(rev(object[["triangles"]][[x]]
		[row(as.matrix(object[["triangles"]][[x]]))
		==(m+1-col(as.matrix(object[["triangles"]][[x]])))])[-1])})
	Latest <- lapply(1:p, function(x){
		c(Latest[[x]],sum(Latest[[x]]))
		})
	
	Ultimate <- lapply(1:p, function(x){
		c(object$fullTriangles[[x]][-(1:(nAdd+1)),n])
		})
	Ultimate <- lapply(1:p, function(x){
		c(Ultimate[[x]],sum(Ultimate[[x]]))
		})	

	s <- (nAdd+1)*p
	
	mse.ay.ult <- object$mse.ay[-(1:s),((n-1)*p+1):(p*n)]
	mse.ay.ult.est <- object$mse.ay.est[-(1:s),((n-1)*p+1):(p*n)]
	mse.ay.ult.proc <- object$mse.ay.proc[-(1:s),((n-1)*p+1):(p*n)]
	mse.total.ult <- object$mse.total[,((n-1)*p+1):(p*n)]
	mse.total.ult.est <- object$mse.total.est[,((n-1)*p+1):(p*n)]
	mse.total.ult.proc <- object$mse.total.proc[,((n-1)*p+1):(p*n)]
	
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
		}	

	else {
		Latest[[p+1]] <- Reduce("+",Latest[agg])
		Ultimate[[p+1]] <- Reduce("+",Ultimate[agg])
		IBNR <- lapply(1:(p+1),function(x) Ultimate[[x]]-Latest[[x]])
		Dev.To.Date <- lapply(1:(p+1),function(x) Latest[[x]]/Ultimate[[x]])
		mse.ult.matrix <- rbind(mse.ay.ult,mse.total.ult)
		mse.ult <- lapply(1:p, function(x) mse.ult.matrix[((1:(dim(mse.ult.matrix)[1]/p)-1)*p+x),x])
		mse.ult[[p+1]] <- sapply(1:(dim(mse.ult.matrix)[1]/p), 
							function(x) sum(mse.ult.matrix[(agg+(x-1)*p),agg]))
							
		mse.ult.est.matrix <- rbind(mse.ay.ult.est,mse.total.ult.est)
		mse.ult.est <- lapply(1:p, function(x) mse.ult.est.matrix[((1:(dim(mse.ult.est.matrix)[1]/p)-1)*p+x),x])
		mse.ult.est[[p+1]] <- sapply(1:(dim(mse.ult.est.matrix)[1]/p), 
							function(x) sum(mse.ult.est.matrix[(agg+(x-1)*p),agg]))
							
		mse.ult.proc.matrix <- rbind(mse.ay.ult.proc,mse.total.ult.proc)
		mse.ult.proc <- lapply(1:p, function(x) mse.ult.proc.matrix[((1:(dim(mse.ult.proc.matrix)[1]/p)-1)*p+x),x])
		mse.ult.proc[[p+1]] <- sapply(1:(dim(mse.ult.proc.matrix)[1]/p), 
							function(x) sum(mse.ult.proc.matrix[(agg+(x-1)*p),agg]))														
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
		nm <- paste("Summary Statistics for Triangle",c(as.character(1:p), paste(agg,sep="",collapse="+")),sep=" ")
		names(output) <- nm
		}
	
	return(output)    
}



print.GeneralMultiChainLadder <- function(x,...){
	
    summary.x <- summary.GeneralMultiChainLadder(x)
    cat("\n")
    print(summary.x, quote=FALSE)
    invisible(x)

}


# transform the Bcov from mcl to gmcl format
transf.Bcov <- function(Bcov.orig){
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

#extract certain columns and put them to triangle format
extract.cols <- function(da.list, cols){
	p <- length(da.list)
	da.list2 <- lapply(1:p,function(x) {
		sel <- da.list[[x]][,cols]
		s <- apply(is.na(sel),1,sum)
		sel <- sel[s<length(cols),]
		return(sel)
		} )
	return(da.list2)
	}
	
