# Copyright 2008 Markus Gesmann, Markus.Gesmann@web.de
LogReserve <- function(triangle, N=10000, 
						tail=1,tail.sd=0){
							
	n <- ncol(triangle)
	idv.f <- log(triangle[,-1]/triangle[,-n])
	mean.f <- apply(idv.f, 2, mean, na.rm=TRUE)
	sd.f <- apply(idv.f, 2, sd, na.rm=TRUE)
	var.f <- apply(idv.f, 2, var, na.rm=TRUE)
	var.f[n-1] <- var.f[(n-2)]
	cum.mean.f <- c(rev(cumsum(rev(mean.f))),log(tail))
	cum.sd.f <- sqrt(rev(cumsum(rev(var.f))))	
	cum.sd.f <- c(cum.sd.f,tail.sd)
	
	Latest <- triangle[row(triangle) == (n+1 - col(triangle))]
	
	#probs=c(0.75, 0.9)
	#Ult <- apply(as.matrix(probs),1, 
	#			qlnorm, cum.mean.f, cum.sd.f)*Latest
	#Ult <- Ult[n:1,]			
	#dimnames(Ult) <- list(origin=dimnames(triangle)[[1]],
	#					 quantile=probs)
 normal.logmu <- function(logmu, logsig){exp(logmu + 1/2 * logsig^2)}
 normal.logsig <- function(logmu, logsig){sqrt(exp(2*logmu + logsig^2)*(exp(logsig^2) - 1))}

 IBNR.mean <-  (normal.logmu(cum.mean.f, cum.sd.f) - 1)* Latest
 IBNR.sd <-  normal.logsig(cum.mean.f, cum.sd.f) * Latest
						 
	r.cum.mean.f <- apply(data.frame(cum.mean.f, cum.sd.f),1,
					function(x) rlnorm(N, x[1],x[2]))
    IBNR <- t(t(r.cum.mean.f - 1)*Latest)
    colnames(IBNR) <- rev(dimnames(triangle)[[1]])
    
    result <- list()
    result[["Triangle"]] <- triangle
    result[["Latest"]] <- Latest
    result[["IBNR"]] <- IBNR
    result[["mean.f"]] <- mean.f
    result[["sd.f"]] <- sd.f					
	class(result) <- c("LogReserve", class(result))
	return(result)						
	}

summary.LogReserve <- function(object, probs=c(0.75,0.9,0.99),...){
	IBNR <- object[["IBNR"]]
	Latest <- object[["Latest"]]
	IBNR.mean <- apply(IBNR,2,mean)
	IBNR.sd <- apply(IBNR,2,sd)
	IBNR.CoV <- IBNR.sd/IBNR.mean
	IBNR.quantile <- apply(IBNR,2,quantile, probs)
	qnames <- rownames(IBNR.quantile)
	IBNR.quantile <- t(IBNR.quantile)
	
	Ultimate <- IBNR.mean+Latest
	Result <- data.frame(Latest, Ultimate, IBNR.mean, IBNR.sd,
				IBNR.CoV, 
				IBNR.quantile)
	names(Result)[6:ncol(Result)] <- qnames			
	Result <- Result[nrow(Result):1,]
	return(Result)
	}

print.LogReserve <- function(object,probs=c(0.75,0.9,0.99),...){
	res <- summary(object,probs=probs,...)
	print(format(res, big.mark = ",", digits = 3), ...)
	IBNR <- object[["IBNR"]]
	Latest <- object[["Latest"]]
	#Totals
	Total.IBNR.mean <- mean(apply(IBNR,1,sum))
	Total.IBNR.sd <- sd(apply(IBNR,1,sum))
	Total.IBNR.CoV <- Total.IBNR.sd/Total.IBNR.mean*100
	Total.IBNR.quantile <- quantile(apply(IBNR,1,mean), probs)
	Total.Latest <- sum(Latest)
	Total.Ultimate <- Total.Latest + Total.IBNR.mean
	
   Totals <- c(Total.Latest, Total.Ultimate, Total.IBNR.mean,
    		   Total.IBNR.sd, Total.IBNR.CoV)
    Totals <- formatC(Totals, big.mark = ",", digit = 0, format = "f")
    Totals <- as.data.frame(Totals)
    colnames(Totals) = c("Totals:")
    rownames(Totals) <- c("Sum of Latest:", "Sum of Ultimate:", 
        "Sum of IBNR:", "Total IBNR sd:", "Total IBNR CoV %:")
    cat("\n")
    print(Totals, quote = FALSE)
	}
	
plot.LogReserve <- function(x,...){
	triangle <- x[["Triangle"]]
	n <- ncol(triangle)
	idv.f <- log(triangle[,-1]/triangle[,-n])
    mean.f <- x[["mean.f"]] #apply(idv.f, 2, mean, na.rm=TRUE)
    matplot(t(idv.f), t="b", ylab="log(Age-to-age factors)", xlab="dev.")
    lines(mean.f, lwd=2)
    sd.f <- x[["sd.f"]] #apply(idv.f, 2, sd, na.rm=TRUE)
    lines(mean.f+sd.f, lty=3)
    lines(mean.f-sd.f, lty=3)
   # print(   c(sort((mean.f+sd.f),FALSE), sort((mean.f-sd.f),TRUE)))
   # polygon((c(1:(n-1), 1:(n-1)	)), c((mean.f+sd.f), (mean.f-sd.f)), col="#FFFFCC", border=NA)
#	plot(ecdf(x[["IBNR"]]),main="IBNR ecdf", xlab="IBNR")

 #ldf <- as.list(as.data.frame(x))
 #lapply(names(ldf), function(.x) plot(ecdf(ldf[[.x]]), main=.x, xlim=c(0,50000)))


# plot(ecdf(idv.f[,1][-10]))
 # fitdistr(idv.f[,1][-10], "lognormal")
# curve(plnorm(x, 0.218, 0.64), add=TRUE, col=2)

# plot(ecdf(cum.mean.f))
# fitdistr(cum.mean.f[-10], "lognormal")
# curve(plnorm(x, -1.69, 1.73), add=TRUE, col=2)

	}
	