
###################################################################################
# Main function to fit model, predict, and calculate mse under MCL

mcl <- function(triangles,
                fit.method="SUR",
                delta=1,
                extrap="Mack",
                mse.method="Mack",...){

    ## Check if the dimensions of all triangles are equal
    if(!check.triangles.dimensions(triangles)){
        stop("Please ensure all triangles have the same dimensions.\n")
    }

    p <- length(triangles)
    m <- dim(triangles[[1]])[1]
    n <- dim(triangles[[1]])[2]

    ## fit the mcl model
    models <- fit.mcl(triangles=triangles,
                      fit.method =fit.method,
                      delta=delta,
                      extrap=extrap,...)

    ## fill the triangle
    fullTriangles <- predict.mclmodels(models)

    ## calculate the mse
    mse <- mse.mcl(mse.method=mse.method,
                   models=models,
                   fullTriangles=fullTriangles)


    output <- c(models,mse)

    class(output) <- c("gmcl","list")
    return(output)
}

######################################################################################
# Fit regression models

fit.mcl=function(triangles,
			fit.method="SUR",
			delta=1,
			extrap="Mack",...)
	{
	p=length(triangles)
	m=dim(triangles[[1]])[1]
	n=dim(triangles[[1]])[2]

	B=Bcov=ecov=myModel=vector("list",n-1)

		for (i in 1:(n-1)){
			y=x=system=vector("list",p)
			for (j in 1:p){
				y[[j]]=triangles[[j]][1:(m-i),i+1]*(triangles[[j]][1:(m-i),i]^(-delta/2))
				x[[j]]=triangles[[j]][1:(m-i),i]*(triangles[[j]][1:(m-i),i]^(-delta/2))
				system[[j]]=as.formula(paste("y[[",j,"]]~-1+x[[",j,"]]",sep=""))

			}
			if (!(i==n-1 & extrap!=FALSE )){
				myModel[[i]]=systemfit(system,fit.method,...)
				B[[i]]= as.vector(coef(myModel[[i]]))
				Bcov[[i]]=unname(vcov(myModel[[i]]))
				ecov[[i]]=unname(myModel[[i]]$residCov)
				if (fit.method %in% "OLS") ecov[[i]]=diag(diag(ecov[[i]]),nrow=p)
				# replace off-diagonal elements as 0
			}
			else {
				B[[i]]=sapply(1:p,function(x) triangles[[x]][1,i+1]/triangles[[x]][1,i])
				if (extrap %in% "Genvar"){
					ecov[[i]]=(det(ecov[[i-1]])^2/det(ecov[[i-2]]))^(1/p)*ecov[[i-1]]
				}
				if (extrap %in% "Mack"){
					r=abs(ecov[[i-1]]^2/ecov[[i-2]])
					ecov[[i]]=pmin(abs(ecov[[i-2]]),abs(ecov[[i-1]]),replace(r,is.na(r),0))
				}
				Bcov[[i]]=solve(diag(unlist(x),nrow=p)%*%solve(ecov[[i]])%*%diag(unlist(x),nrow=p))
				myModel[[i]]="Not applicable because of extrapolation"
			}
		}
	output=list()
	output[["triangles"]]=triangles
	output[["models"]]=myModel
	output[["B"]]=B
	output[["Bcov"]]=Bcov
	output[["ecov"]]=ecov
	output[["fit.method"]]=fit.method
	output[["delta"]]=delta
	output[["extrap"]]=extrap
	class(output)="mclmodels"
	return(output)
}

########################################################################################
# Complete the triangles

predict.mclmodels=function(object,...)
	{
	triangles=object$triangles
	B=object$B
	p=length(triangles)
	m=dim(triangles[[1]])[1]
	n=dim(triangles[[1]])[2]
	fullTriangles=triangles
    	for (i in 1:(n-1)){
        	x=sapply(1:p, function(x) fullTriangles[[x]][(m-i+1):m,i])
        	x=matrix(x,p,i,byrow=TRUE)
			y=diag(B[[i]],nrow=p)%*%x
			for (j in 1:p){
				fullTriangles[[j]][(m-i+1):m,(i+1)]=y[j,]
			}
		}
	return(fullTriangles)
	}


########################################################################################
# Compute the mse for each accident year


mse.mcl=function(mse.method="Mack", models, fullTriangles)
	{
	triangles=models$triangles
	p=length(fullTriangles)
	n=ncol(fullTriangles[[1]])
	m=nrow(fullTriangles[[1]])
	B=models$B
	Bcov=models$Bcov
	ecov=models$ecov
	delta=models$delta

	mse.ay=mse.ay.est=mse.ay.proc=matrix(0,m*p,n*p)
	mse.total=mse.total.est=mse.total.proc=matrix(0,p,p*n)


	# mse for single accident years
	for ( k in 1:(n-1))
		{
		for (i in m:(m+1-k))
			{
			a1=(p*(i-1)+1):(p*i)	 #old indexes
			b1=(p*(k-1)+1):(p*k)
			a2=(p*(i-1)+1):(p*i)	# new indexes
			b2=(p*k+1):(p*(k+1))

			yhat=sapply(1:p,function(x) fullTriangles[[x]][i,k])
			#process variance
			mse.ay.proc[a2,b2]=diag(yhat^(delta/2),nrow=p)%*%ecov[[k]]%*%diag(yhat^(delta/2),nrow=p)+
								B[[k]]%*%t(B[[k]])*mse.ay.proc[a1,b1]

			#estimation variance
			if (mse.method %in% "Mack")	#Mack formulas
				{
				mse.ay.est[a2,b2]=Bcov[[k]]*(yhat%*%t(yhat))+
									(B[[k]]%*%t(B[[k]]))*mse.ay.est[a1,b1]
				}
			if (mse.method %in% "Murphy") #generalized Murphy formulas
				{
				mse.ay.est[a2,b2]=Bcov[[k]]*(yhat%*%t(yhat))+
									(B[[k]]%*%t(B[[k]]))*mse.ay.est[a1,b1]+
									Bcov[[k]]*mse.ay.est[a1,b1]
				}
			#mse
			mse.ay[a2,b2]=mse.ay.proc[a2,b2]+mse.ay.est[a2,b2]
			}
		}

	# mse for aggregated accident years
	for (k in 1:(n-1))
		{

		b1=(p*(k-1)+1):(p*k)
		b2=(p*k+1):(p*(k+1))
		proc=matrix(0,p,p)

		#process variance
		for (i in ((m+1-k):m))
			{
			yhat=sapply(1:p,function(x) fullTriangles[[x]][i,k])
			proc=proc+diag(yhat^(delta/2),nrow=p)%*%ecov[[k]]%*%diag(yhat^(delta/2),nrow=p)
			}

		sumyhat=sapply(1:p,function(x) sum(fullTriangles[[x]][(m+1-k):m,k]))
		mse.total.proc[,b2]=proc+B[[k]]%*%t(B[[k]])*mse.total.proc[,b1]

		#estimation variance
		if (mse.method == "Mack")
			{
			mse.total.est[,b2]=Bcov[[k]]*(sumyhat%*%t(sumyhat))+
									(B[[k]]%*%t(B[[k]]))*mse.total.est[,b1]
			}
		if (mse.method == "Murphy")
			{
			mse.total.est[,b2]=Bcov[[k]]*(sumyhat%*%t(sumyhat))+
									(B[[k]]%*%t(B[[k]]))*mse.total.est[,b1]+
									Bcov[[k]]*mse.total.est[,b1]
			}

		#total variance
		mse.total[,b2]=mse.total.proc[,b2]+mse.total.est[,b2]
		}


	return(list(mse.ay=mse.ay,
				mse.ay.est=mse.ay.est,
				mse.ay.proc=mse.ay.proc,
				mse.total=mse.total,
				mse.total.est=mse.total.est,
				mse.total.proc=mse.total.proc,
				fullTriangles=fullTriangles,
				mse.method=mse.method))
	}

check.triangles.dimensions <- function(triangles){
## Check if the dimensions of all triangles are equal
    dims <- sapply(triangles, dim)
    all( dims - apply(dims, 1, mean) ==0)
}
