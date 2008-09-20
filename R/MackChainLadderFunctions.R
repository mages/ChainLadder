
## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:10/11/2007
## Date:08/09/2008



MackChainLadder <- function(Triangle, weights=1/Triangle, tail=FALSE){

    cTriangle <- checkTriangle(Triangle)
    m <- cTriangle$m
    n <- cTriangle$n
    Triangle <- cTriangle$Triangle

    myModel <- ChainLadder(Triangle, weights)$Models
    ## Predict the chain ladder model
    FullTriangle <- predict.TriangleModel(list(Models=myModel, Triangle=Triangle))

    ## Estimate the standard error
    StdErr <- Mack.S.E(myModel, FullTriangle, loglinear=TRUE)
    Total.SE <- TotalMack.S.E(FullTriangle, StdErr$f, StdErr$f.se, StdErr$F.se)

    ## Check for tail factor
    if(is.logical(tail)){
        if(tail){
            tail <- tailfactor(StdErr$f)
            tail.factor <- tail$tail.factor
        }else{
            tail.factor <- tail
        }
    }else{
        tail.factor <- tail
    }

    ## Estimate standard error for the tail
    if(tail.factor>1){
        tail.out <- tail.SE(FullTriangle, StdErr, Total.SE, tail.factor)
        FullTriangle <- tail.out[["FullTriangle"]]
        StdErr <- tail.out[["StdErr"]]
        Total.SE <- tail.out[["Total.SE"]]
    }

    ## Collect the output
    output <- list()
    output[["call"]] <-  match.call(expand.dots = FALSE)
    output[["Triangle"]] <- Triangle
    output[["FullTriangle"]] <- FullTriangle
    output[["Models"]] <- myModel
    output[["f"]] <- StdErr$f
    output[["f.se"]] <- StdErr$f.se
    output[["F.se"]] <- StdErr$F.se
    output[["sigma"]] <- StdErr$sigma
    output[["Mack.S.E"]] <- StdErr$FullTriangle.se
    output[["Total.Mack.S.E"]] <- Total.SE
    output[["tail"]] <- tail

    class(output) <- c("MackChainLadder", "TriangleModel", "list")
    return(output)
}
########################################################################
## Estimate standard error for tail

tail.SE <- function(FullTriangle, StdErr, Total.SE, tail.factor){
    n <- ncol(FullTriangle)
    m <- nrow(FullTriangle)
    FullTriangle[,n] <- FullTriangle[,n] * tail.factor
    StdErr$f[n] <- tail.factor
    StdErr$f.se[n] <- mack.se.fult(clratios = StdErr$f, se.f = StdErr$f.se)
    StdErr$F.se <- cbind(StdErr$F.se, mack.se.Fult(se.fult = StdErr$f.se[n], se.F = StdErr$F.se))
    StdErr$FullTriangle.se[,n] <- sqrt(FullTriangle[, n]^2 * (StdErr$F.se[,n]^2 + StdErr$f.se[n]^2) +
                                       StdErr$FullTriangle.se[,n]^2 * tail.factor^2)
    Total.SE <- sqrt(Total.SE^2 * StdErr$f[n]^2 + sum(FullTriangle[c(1:m), n]^2 * (StdErr$F.se[c(1:m), n]^2), na.rm=TRUE) +
                     sum(FullTriangle[c(1:m), n], na.rm=TRUE)^2 * StdErr$f.se[n]^2)
    output <- list(FullTriangle=FullTriangle, StdErr=StdErr, Total.SE=Total.SE)
    return(output)
}



##############################################################################
## Calculation of the mean squared error and standard error
## mean squared error = stochastic error (process variance) + estimation error
## standard error = sqrt(mean squared error)

Mack.S.E <- function(MackModel, FullTriangle, loglinear=TRUE){
    n <- ncol(FullTriangle)
    m <- nrow(FullTriangle)
    f <- rep(1,n)
    f.se <- rep(0,n)
    sigma <- rep(0,(n-1))

    ## Extract estimated slopes, std. error and sigmas
    f[1:(n-1)] <- sapply(MackModel, function(x) summary(x)$coef["x","Estimate"])
    f.se[1:(n-1)] <- sapply(MackModel, function(x) summary(x)$coef["x","Std. Error"])
    sigma[1:(n-1)] <- sapply(MackModel, function(x) summary(x)$sigma)


    if(loglinear){
        ## estimate sigma[n-1] via log-linear regression
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
    ## Recursive Formula
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
## Summary
##
summary.MackChainLadder <- function(object,...){
    ## Summarise my results
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
## print
##
print.MackChainLadder <- function(x,...){

    res <- summary(x)
    print(x$call)
    cat("\n")
    print(format(res[!is.na(res$Latest),], big.mark = ",", digits = 3),...)
    Totals <-  c(sum(res$Latest,na.rm=TRUE), sum(res$Ultimate,na.rm=TRUE),
                 sum(res$IBNR,na.rm=TRUE), x[["Total.Mack.S.E"]]
                 #,x[["Total.Mack.S.E"]]/sum(res$IBNR,na.rm=TRUE)
                 )
    Totals <- formatC(Totals, big.mark=",",digits=0,format="f")
    Totals <- c(Totals, round(x[["Total.Mack.S.E"]]/sum(res$IBNR,na.rm=TRUE),2))
    Totals <- as.data.frame(Totals)

    colnames(Totals)=c("Totals")
    rownames(Totals) <- c("Latest:","Ultimate:",
                          "IBNR:","Mack S.E.:",
                          "CoV:")
    cat("\n")
    print(Totals, quote=FALSE)
    #invisible(x)

}


################################################################################
## plot
##
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
                  xlab="Origin period",
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
            main="Chain ladder developments by origin period",
            xlab="Development period",
            ylab="Amounts", #paste(Currency, myUnit)
            )
    matplot(t(.Triangle), add=TRUE)

    Residuals=residuals(x)
    plot(standard.residuals ~ fitted.value, data=Residuals,
         ylab="Standardised residuals", xlab="Fitted")
    lines(lowess(Residuals$fitted.value, Residuals$standard.residuals), col="red")
    abline(h=0, col="grey")
    plot(standard.residuals ~ origin.period, data=Residuals,
         ylab="Standardised residuals", xlab="Origin period")
    lines(lowess(Residuals$origin.period, Residuals$standard.residuals), col="red")
    abline(h=0, col="grey")
    plot(standard.residuals ~ cal.period, data=Residuals,
         ylab="Standardised residuals", xlab="Calendar period")
    lines(lowess(Residuals$cal.period, Residuals$standard.residuals), col="red")
    abline(h=0, col="grey")
    plot(standard.residuals ~ dev.period, data=Residuals,
         ylab="Standardised residuals", xlab="Development period")
    lines(lowess(Residuals$dev.period, Residuals$standard.residuals), col="red")
    abline(h=0, col="grey")

    title( title , outer=TRUE)
    par(op)
}
################################################################################
## residuals
##
residuals.MackChainLadder <- function(object,...){
    n <- nrow(object[["Triangle"]])
    myresiduals <- unlist(lapply(object[["Models"]], resid,...))
    standard.residuals <- unlist(lapply(object[["Models"]], rstandard,...))
    fitted.value <- unlist(lapply(object[["Models"]], fitted))
    origin.period <- unlist(lapply(1:(n-1), function(x) 1:length(resid( object[["Models"]][[x]]) )))
    dev.period <- unlist(lapply(1:(n-1), function(x) rep(x,length(resid( object[["Models"]][[x]]) ))))
    cal.period <- origin.period + dev.period - 1

    myResiduals=data.frame(origin.period,
    dev.period,
    cal.period,
    residuals=myresiduals,
    standard.residuals,
    fitted.value)
    return(na.omit(myResiduals))
}


##############################################################################
## estimate the stanard error of the tail factor

mack.se.fult <- function (clratios, se.f){
    f <- clratios
    n <- length(f)
    fult <- f[n]
    k <- findInterval(fult, sort(f[-n]))
    if (k != 0) {
        k <- order(f[-n])[k]
    }
    if ((1 < k) && (k < n)) {
        if (abs(f[k] - f[k - 1]) > 0) {
            se.fult <- (1 - (f[k] - fult)/(f[k] - f[k - 1])) *
                se.f[k - 1] + (f[k] - fult)/(f[k] - f[k - 1]) *
                    se.f[k]
        }
        else {
            se.fult <- se.f[n - 1]
        }
    }
    else {
        se.fult <- se.f[n - 1]
    }
    return(se.fult)
}

##############################################################################
## estimate the stanard error of the tail factor ratios


mack.se.Fult <- function (se.fult, se.F){
    n <- ncol(se.F)
    se.Fult <- se.F[, n]
    se.Fult <- as.matrix(se.Fult)
    se.Fult <- se.fult * (1 + se.Fult)
    return(se.Fult)
}


