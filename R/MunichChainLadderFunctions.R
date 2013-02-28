## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:10/11/2007; 17/09/2008

##############################################################################
## Get the top left triangle of a matrix
left.tri <- function(x) col(as.matrix(x)) < ncol(as.matrix(x))-row(as.matrix(x)) + 2
##############################################################################



##############################################################################
## getMCLResiduals
## transform a triangle of residuals into a vector as needed by MunichChainLadder

getMCLResiduals <- function(res, n){
     x <- matrix(NA,n,n)
     my.tri <-function(x) col(as.matrix(x)) < ncol(as.matrix(x))-row(as.matrix(x)) + 1
     .ind <- which(my.tri(x), TRUE)
     x[.ind] <- res[.ind]
     x[,(n-1):n] <- NA

     return(as.vector(x))
 }

##############################################################################
## Munich Chain Ladder
##
MunichChainLadder <- function(Paid, Incurred,
                              est.sigmaP="log-linear",
                              est.sigmaI="log-linear",
                              tailP=FALSE,
                              tailI=FALSE){

    if(!all(dim(Paid) == dim(Incurred)))
 	stop("Paid and Incurred triangle must have same dimension.\n")

    n <- ncol(Paid)
    m <- nrow(Paid)
    
    if(m > n)
      stop("MunichChainLadder does not support triangles with fewer development periods than origin periods.\n")
       
    MackPaid = MackChainLadder(Paid, tail=tailP, est.sigma=est.sigmaP)
    MackIncurred = MackChainLadder(Incurred, tail=tailI, est.sigma=est.sigmaI)

       
    myQModel <- vector("list", n)
    q.f <- rep(1,n)
    rhoI.sigma <- rep(0,n)

    for(s in c(1:n)){
 	myQModel[[s]] <- lm(Paid[1:(n-s+1),s] ~ Incurred[1:(n-s+1),s] + 0,
                            weights=1/Incurred[1:(n-s+1),s])

        q.f[s] <- summary(myQModel[[s]])$coef[1]
	rhoI.sigma[s] <- summary(myQModel[[s]])$sigma
    }
    rhoI.sigma <-  estimate.sigma(rhoI.sigma)$sigma

    myQinverseModel <- vector("list", n)
    qinverse.f <- rep(1,n)
    rhoP.sigma <- rep(0,n)

    for(s in c(1:n)){
        myQinverseModel[[s]] <- lm(Incurred[1:(n-s+1),s] ~ Paid[1:(n-s+1),s] + 0,
                                   weights=1/Paid[1:(n-s+1),s])

	qinverse.f[s] = summary(myQinverseModel[[s]])$coef[1]
	rhoP.sigma[s] = summary(myQinverseModel[[s]])$sigma
    }
    rhoP.sigma <- estimate.sigma(rhoP.sigma)$sigma

    ## Estimate the residuals

    Paidf <-  t(matrix(rep(MackPaid$f[-n],(m-1)), ncol=(m-1)))
    PaidSigma <- t(matrix(rep(MackPaid$sigma,(m-1)), ncol=(m-1)))
    PaidRatios <- Paid[-m,-1]/Paid[-m,-n]
    PaidResiduals <- (PaidRatios - Paidf)/PaidSigma[, 1:ncol(Paidf)] * sqrt(Paid[-m,-n])

    Incurredf <-  t(matrix(rep(MackIncurred$f[-n],(m-1)), ncol=(m-1)))
    IncurredSigma <- t(matrix(rep(MackIncurred$sigma,(m-1)), ncol=(m-1)))
    IncurredRatios <- Incurred[-m,-1]/Incurred[-m,-n]
    IncurredResiduals <- (IncurredRatios - Incurredf)/IncurredSigma[,1:ncol(Incurredf)] * sqrt(Incurred[-m,-n])

    QRatios <- (Paid/Incurred)[,-n]
    Qf <- t(matrix(rep(q.f[-n],m), ncol=m))
    QSigma <- t(matrix(rep(rhoI.sigma[-n],m), ncol=m))
    QResiduals <- (QRatios - Qf)/QSigma * sqrt(Incurred[,-n])

    QinverseRatios <- 1/QRatios
    Qinversef <- 1/Qf
    QinverseSigma <- t(matrix(rep(rhoP.sigma[-n],m), ncol=m))
    QinverseResiduals <- (QinverseRatios - Qinversef)/QinverseSigma * sqrt(Paid[,-n])

    QinverseResiduals <-  getMCLResiduals(QinverseResiduals,n)
    QResiduals <-  getMCLResiduals(QResiduals,n)
    IncurredResiduals <-  getMCLResiduals(IncurredResiduals,n)
    PaidResiduals <-  getMCLResiduals(PaidResiduals,n)

    ## linear regression of the residuals through the origin
    inc.res.model <- lm(IncurredResiduals ~ QResiduals+0)
    lambdaI <- coef(inc.res.model)[1]

    paid.res.model <- lm(PaidResiduals ~ QinverseResiduals+0)
    lambdaP <- coef(paid.res.model)[1]


    ## Recursive Munich Chain Ladder Forumla
    FullPaid <- cbind(Paid, rep(NA,m))
    FullIncurred <- cbind(Incurred, rep(NA,m))
#prn(n)
#prn(FullPaid)
#    FullPaid <- Paid
#    FullIncurred <- Incurred

    for(j in c(1:(n))){
        for(i in c((n-j+1):m) ){# 3:4, 2:4, 1:4
          ## Check for triangles with n<m
##          if(m < n | (i > (m+1-j))){
           
            ## Paid
            mclcorrection <- lambdaP*MackPaid$sigma[j]/rhoP.sigma[j]*(
                                                                      FullIncurred[i,j]/FullPaid[i,j]-qinverse.f[j]
                                                                      )
            mclcorrection <- ifelse(!is.na(mclcorrection),mclcorrection,0)
              FullPaid[i,j+1] = FullPaid[i,j] * (MackPaid$f[j] + mclcorrection)
            ## Incurred
            mclcorrection <- lambdaI*MackIncurred$sigma[j]/rhoI.sigma[j]*(
                                                                          FullPaid[i,j]/FullIncurred[i,j]-q.f[j]
                                                                          )
            mclcorrection <- ifelse(!is.na(mclcorrection),mclcorrection,0)
              FullIncurred[i,j+1] = FullIncurred[i,j] * (MackIncurred$f[j] + mclcorrection)
    ##        }
          }
      }

    output <- list()
    output[["call"]] <-  match.call(expand.dots = FALSE)
    output[["Paid"]] <- Paid
    output[["Incurred"]] <- Incurred
    output[["MCLPaid"]] <- FullPaid[,c(1:(n-1), n+1)]
    output[["MCLIncurred"]] <- FullIncurred[,c(1:(n-1), n+1)]
    output[["MackPaid"]] <- MackPaid
    output[["MackIncurred"]] <- MackIncurred
    output[["PaidResiduals"]] <-  PaidResiduals
    output[["IncurredResiduals"]] <- IncurredResiduals
    output[["QResiduals"]] <-  QResiduals
    output[["QinverseResiduals"]] <- QinverseResiduals
    output[["lambdaP"]] <- paid.res.model
    output[["lambdaI"]] <- inc.res.model
    output[["qinverse.f"]] <- qinverse.f
    output[["rhoP.sigma"]] <- rhoP.sigma
    output[["q.f"]] <- q.f
    output[["rhoI.sigma"]] <- rhoI.sigma


    class(output) <- c("MunichChainLadder", "list")

    return(output)

}

##############################################################################
## summary
##
summary.MunichChainLadder <- function(object,...){
    n <- ncol(object[["MCLPaid"]])
    m <- nrow(object[["MCLPaid"]])

    .Paid <- as.matrix(object[["Paid"]])
    .Incurred <- as.matrix(object[["Incurred"]])

    getCurrent <- function(.x){
            rev(.x[row(as.matrix(.x)) == (nrow(.x)+1 - col(as.matrix(.x)))])
        }

    if(m > n){
        LatestPaid <- c(.Paid[1:(m-n),n], getCurrent(.Paid[(m-n+1):m,]))
        LatestIncurred <- c(.Incurred[1:(m-n),n], getCurrent(.Incurred[(m-n+1):m,]))
    }else{
        LatestPaid <- getCurrent(.Paid)
        LatestIncurred <- getCurrent(.Incurred)
    }

    UltimatePaid = object[["MCLPaid"]][,ncol(object[["MCLPaid"]])]
    UltimateIncurred = object[["MCLIncurred"]][,ncol(object[["MCLIncurred"]])]

    ex.origin.period <- !is.na(LatestIncurred)

    ByOrigin <- data.frame(LatestPaid,	LatestIncurred,
                         Latest.P.I.Ratio=LatestPaid/LatestIncurred,
                         UltimatePaid,
                         UltimateIncurred,
                         Ultimate.P.I.Ratio=UltimatePaid/UltimateIncurred)
    ByOrigin <- ByOrigin[ex.origin.period,]
    names(ByOrigin) <- c("Latest Paid",
                         "Latest Incurred",
                         "Latest P/I Ratio",
                         "Ult. Paid",
                         "Ult. Incurred",
                         "Ult. P/I Ratio")

    Totals <- data.frame(Paid=c(sum(LatestPaid, na.rm=TRUE),
                         sum(UltimatePaid, na.rm=TRUE)),
                         Incurred=c(sum(LatestIncurred, na.rm=TRUE),
                         sum(UltimateIncurred, na.rm=TRUE))
                         )
    Totals["P/I Ratio"] <- with(Totals, Paid/Incurred)
    rownames(Totals) <- c("Latest:", "Ultimate:")

    output <- list(ByOrigin=ByOrigin, Totals=Totals)
    return(output)
}

##############################################################################
## print
##
print.MunichChainLadder <- function(x,...){
    summary.x <- summary(x)
    print(x$call)
    cat("\n")
    print(format(summary.x$ByOrigin, big.mark = ",", digits = 3),...)
    cat("\nTotals\n")
    print(format(summary.x$Totals, big.mark = ",", digits = 2),...)

}

##############################################################################
## Plot
##
plot.MunichChainLadder <- function(x, mfrow=c(2,2), title=NULL, ...){

    if(is.null(title)) myoma <- c(0,0,0,0) else myoma <- c(0,0,2,0)

    op=par(mfrow=mfrow, oma=myoma)
    .origin <- dimnames(x$Paid)[[1]]

    n <- ncol(x[["MCLPaid"]])
    barplot(t(as.matrix(data.frame(Paid=x[["MCLPaid"]][,n], Incurred=x[["MCLIncurred"]][,n]))),
            beside=TRUE, legend.text=c("MCL Paid", "MCL Incurred"), names.arg=.origin,
            xlab="origin period", ylab="Amounts", main="Munich Chain Ladder Results")

    barplot(t(as.matrix(
                        data.frame(SCL.PI=100*x[["MackPaid"]]$FullTriangle[,n]/x[["MackIncurred"]]$FullTriangle[,n],
                                   MCL.PI=100*x[["MCLPaid"]][,n]/x[["MCLIncurred"]][,n]))),
            beside=TRUE, legend.text=c("Mack P/I", "MCL P/I"), names.arg=.origin,
            xlab="origin period", ylab="%", main="Munich Chain Ladder vs. Standard Chain Ladder")


    plot(x[["PaidResiduals"]] ~ x[["QinverseResiduals"]],
         xlim=c(-2,2), ylim=c(-2,2),
         ylab="Paid residuals",
         xlab="Incurred/Paid residuals",
         main="Paid residual plot")
    abline(v=0)
    abline(h=0)
    abline(a=0,b=coef(x[["lambdaP"]])[1], col="red")

    plot(x[["IncurredResiduals"]] ~ x[["QResiduals"]],
         xlim=c(-2,2), ylim=c(-2,2),
         ylab="Incurred residuals",
         xlab="Paid/Incurred residuals",
         main="Incurred residual plot")
    abline(v=0)
    abline(h=0)
    abline(a=0,b=coef(x[["lambdaI"]])[1], col="red")

    title( title , outer=TRUE)
    par(op)
}
