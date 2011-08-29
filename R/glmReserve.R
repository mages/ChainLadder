###################################################
##    Glm-based insurance loss reserving 
##            Wayne Zhang 
##     actuary_zhang@hotmail.com
###################################################

glmReserve <- function(triangle, var.power=1,link.power=0,
                       cum=TRUE, mse.method="formula", nsim=1000,...){
  
  call <- match.call()
  if (!("triangle") %in% class(triangle))
    stop("triangle must be of class 'triangle'")
  if ("offset" %in% names(list(...)))
    stop("'offset' should be passed using the 
         'exposure' attribute of the triangle!")
  if ("weigth" %in% names(list(...)))
    stop("'weight' should not be used")  
  # convert to incremental losses if needed    
  tr.incr <- if (cum) cum2incr(triangle) else triangle    
  # create family
  family <- tweedie(var.power=var.power,link.power=link.power)
  # convert to long format
  lda <-  as.data.frame(tr.incr)
  lda$offset <- if (is.null(attr(tr.incr,"exposure")))
                rep(0,nrow(lda)) else 
                family$linkfun(rep(attr(tr.incr,"exposure"),
                                   as.numeric(table(lda$origin))))
  ldaFit <- subset(lda,!is.na(value))
  ldaOut <- subset(lda,is.na(value))
   
  # fit the model
  glmFit <- glm(value~factor(origin)+factor(dev),
              family=family,
              data=ldaFit,offset=offset,...)

  # dispersion
  phi <- sum(resid(glmFit,type="pearson")^2)/df.residual(glmFit)

  ################################
  ## calculate reserve 
  ################################
  # prediction for each cell              
  yp <- predict(glmFit,newdata=ldaOut,type="response")
  eta <- as.numeric(predict(glmFit,newdata=ldaOut,type="link"))
                
  # sum to get reserve by year
  resMeanAy <- tapply(yp,ldaOut$origin, sum)
  resMeanTot <- sum(resMeanAy)

  ################################
  ## calculate prediction err 
  ################################                
                
  # process variance              
  mseProcAy <-  phi * tapply(yp,ldaOut$origin, 
                             function(x) sum(family$variance(x)))
  mseProcTot <-  phi * sum(family$variance(yp)) 
                
  # estimation variance                
  if (mse.method %in% "formula"){              
    # design matrix for prediction
    Terms <- delete.response(terms(glmFit))
    m <- model.frame(Terms, ldaOut, xlev = glmFit$xlevels)
    X <- model.matrix(Terms, m)              
    # var(eta)
    v <- X %*% vcov(glmFit) %*% t(X)  # vcov has already been scaled                    
    mseEstAy <- sapply(sort(unique(ldaOut$origin)), function(x){
  				        id <- ldaOut$origin==x	
                  # d mu/  d eta from the delta method
  				        muEta <- diag(family$mu.eta(eta[id]),nrow=length(eta[id]))
  				        rv <- sum(muEta %*% v[id,id] %*% t(muEta))  
  				        return(rv)
              })
    mseEstTot <-  sum(diag(family$mu.eta(eta)) %*% v %*% diag(family$mu.eta(eta)))
              
  }
                       
  if (mse.method %in% "bootstrap"){
    resMeanAyB <- matrix(0,length(resMeanAy),nsim)
    resMeanTotB <- rep(0,nsim)
    # loop nsim times 
    for (i in 1:nsim){      
      mu <- fitted(glmFit)
      # while loop to avoid negative generated incremental loss
      ybad <- 1 
      while (ybad){
        rn <- sample(resid(glmFit,type="pearson"),nrow(ldaFit),replace=TRUE)
        yB <- rn * sqrt(family$variance(mu)) + mu
        if (all(yB>=0))  ybad <- 0 
      }
      glmFitB <-  glm(yB~factor(origin)+factor(dev),
              family=family,data=ldaFit,offset=offset,...)
      ypB <- predict(glmFitB,newdata=ldaOut,type="response")
      resMeanAyB[,i] <- as.numeric(tapply(ypB,ldaOut$origin, sum))
      resMeanTotB[i] <- sum(resMeanAyB[,i])
    }
    # compute estimation variance, adjusted by df 
    mseEstAy <- nrow(ldaFit)/df.residual(glmFit) * apply(resMeanAyB,1,var)
    mseEstTot <- nrow(ldaFit)/df.residual(glmFit) * var(resMeanTotB)
  }
  # compile results
  IBNR <- round(c(resMeanAy,resMeanTot))
  S.E <- sqrt(c(mseProcAy,mseProcTot) + c(mseEstAy,mseEstTot))
  CV <- S.E / IBNR
  Latest <- getLatestCumulative(incr2cum(tr.incr))[-1L]
  Latest <- c(Latest,sum(Latest))
  Ultimate <- Latest + IBNR
  resDf <- data.frame(Latest=Latest, Dev.To.Date=Latest/Ultimate,
                      Ultimate=Ultimate, IBNR=IBNR,
                      S.E=S.E, CV=CV)
	row.names(resDf) <- c(as.character(sort(unique(ldaOut$origin))),"total")
  
  # produce fullly projected triangle
  ldaOut$value <- round(yp)
  FullTriangle <- as.triangle(rbind(ldaFit,ldaOut))
  if (cum)
    FullTriangle <- incr2cum(FullTriangle)
  
  # output
  out <- c(list(call=call,summary=resDf,
                Triangle=triangle,
                FullTriangle=FullTriangle,
                scale=phi), 
           glmFit[!(names(glmFit) %in% c("call"))])
  class(out) <- "glm"                     
  return(out)  
}



