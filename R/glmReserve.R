###################################################
##    glm-based insurance loss reserving 
##            Wayne Zhang 
##     actuary_zhang@hotmail.com
###################################################

glmReserve <- function(triangle, var.power = 1, link.power = 0,
                       cum = TRUE, mse.method = c("formula", "bootstrap"), 
                       nsim = 1000, ...){
  
  call <- match.call()
  mse.method <- match.arg(mse.method)
  
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
  if (!is.null(var.power))
    fam <- tweedie(var.power = var.power, link.power = link.power)
  
  # convert to long format
  lda <-  as.data.frame(tr.incr)
  lda$offset <- if (is.null(attr(tr.incr, "exposure")))
                rep(0, nrow(lda)) else 
                fam$linkfun(rep(attr(tr.incr, "exposure"),
                                   as.numeric(table(lda$origin))))
  ldaFit <- subset(lda, !is.na(lda$value)) #warnings?  
  ldaOut <- subset(lda, is.na(lda$value)) 
   
  # fit the model
  if (!is.null(var.power)){
    glmFit <- glm(value ~ factor(origin) + factor(dev),
              family = fam,
              data = ldaFit, offset = offset,...)
    # dispersion
    phi <- with(glmFit, sum(weights * residuals^2) / df.residual)
  } else{ 
    glmFit <- cpglm(value ~ factor(origin) + factor(dev),
                  link = link.power,
                  data = ldaFit, offset = offset, ...)
    phi <- glmFit$phi
    fam <- tweedie(var.power = glmFit$p, link.power = link.power)
  }

  ################################
  ## calculate reserves 
  ################################
  # prediction for each cell              
  yp <- predict(glmFit, newdata = ldaOut, type = "response")
  eta <- as.numeric(predict(glmFit, newdata = ldaOut, type = "link"))
                
  # sum to get reserve by year
  resMeanAy <- tapply(yp, ldaOut$origin, sum)
  resMeanTot <- sum(resMeanAy)

  ################################
  ## calculate prediction errs 
  ################################                
                
  # process variance              
  mseProcAy <-  phi * tapply(yp, ldaOut$origin, 
                             function(x) sum(fam$variance(x)))
  mseProcTot <-  phi * sum(fam$variance(yp)) 
                
  # estimation variance                
  if (mse.method %in% "formula"){              
    # design matrix for prediction
    Terms <- delete.response(terms(glmFit))
    if (class(glmFit)[1] == "cpglm"){
      xlevels <- .getXlevels(Terms, glmFit$model.frame)
    } else { 
      xlevels <- glmFit$xlevels
    }
    m <- model.frame(Terms, ldaOut, xlev = xlevels)
    X <- model.matrix(Terms, m)              
    # var(eta)
    v <- X %*% vcov(glmFit) %*% t(X)  # vcov has already been scaled                    
    mseEstAy <- sapply(sort(unique(ldaOut$origin)), function(x){
  				        id <- ldaOut$origin == x	
                  # d(mu)/d(eta) from the delta method
  				        muEta <- diag(fam$mu.eta(eta[id]), nrow = length(eta[id]))
  				        rv <- sum(muEta %*% v[id,id] %*% t(muEta))  
  				        return(rv)
              })
    mseEstTot <-  sum(diag(fam$mu.eta(eta)) %*% v %*% diag(fam$mu.eta(eta)))
              
  }
                       
  if (mse.method %in% "bootstrap"){
    resMeanAyB <- matrix(0, length(resMeanAy), nsim)
    resMeanTotB <- rep(0, nsim)
    # loop nsim times 
    for (i in 1:nsim){      
      mu <- fitted(glmFit)
      # while loop to avoid negative generated incremental loss
      ybad <- 1 
      while (ybad){
        rn <- sample(resid(glmFit, type = "pearson"), nrow(ldaFit), replace = TRUE)
        yB <- rn * sqrt(fam$variance(mu)) + mu
        if (all(yB >= 0))  ybad <- 0 
      }
      if (!is.null(var.power)){
        glmFitB <-  glm(yB ~ factor(origin) + factor(dev),
                        family = fam, data = ldaFit, offset = offset, ...)
      } else{ 
        stop("Bootstrap for the compound Poisson model not yet implemented!")
      }      
      ypB <- predict(glmFitB, newdata = ldaOut, type = "response")
      resMeanAyB[, i] <- as.numeric(tapply(ypB, ldaOut$origin, sum))
      resMeanTotB[i] <- sum(resMeanAyB[, i])
    }
    # compute estimation variance, adjusted by df 
    mseEstAy <- nrow(ldaFit)/df.residual(glmFit) * apply(resMeanAyB, 1, var)
    mseEstTot <- nrow(ldaFit)/df.residual(glmFit) * var(resMeanTotB)
  }
  # compile results
  IBNR <- round(c(resMeanAy, resMeanTot))
  S.E <- sqrt(c(mseProcAy, mseProcTot) + c(mseEstAy, mseEstTot))
  CV <- S.E / IBNR
  Latest <- getLatestCumulative(incr2cum(tr.incr))[-1L]
  Latest <- c(Latest, sum(Latest))
  Ultimate <- Latest + IBNR
  resDf <- data.frame(Latest = Latest, Dev.To.Date = Latest/Ultimate,
                      Ultimate = Ultimate, IBNR = IBNR,
                      S.E = S.E, CV = CV)
	row.names(resDf) <- c(as.character(sort(unique(ldaOut$origin))), "total")
  
  # produce fullly projected triangle
  ldaOut$value <- round(yp)
  FullTriangle <- as.triangle(rbind(ldaFit, ldaOut))
  if (cum)
    FullTriangle <- incr2cum(FullTriangle)
  
  # output
  out <- c(list(call = call, summary = resDf,
                Triangle = triangle,
                FullTriangle = FullTriangle,
                model = glmFit))
  class(out) <- "glmReserve"
  return(out)  
}

# summary and print method for glmReserve
summary.glmReserve <- function(object, type = c("triangle", "model"), ...){
  type <- match.arg(type)
  if (type == "triangle")
    print(object$summary)
  if (type == "model")
    summary(object$model)
}

print.glmReserve <- function(x, ...)
  summary(x)


