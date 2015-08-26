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
  fam <- tweedie(ifelse(!is.null(var.power), var.power, 1.5), link.power)
  
  # convert to long format
  lda <-  as.data.frame(tr.incr, origin=names(dimnames(tr.incr))[1], 
                        dev=names(dimnames(tr.incr))[2])
  names(lda)[1:3] <- c("origin", "dev", "value")
  
  # create offset
  if (is.null(attr(tr.incr, "exposure"))) {
    lda$offset <-  rep(0, nrow(lda))
  } else {
    lda$offset <- fam$linkfun(attr(tr.incr, "exposure")[lda$origin - min (lda$origin) + 1])
  }
  
  # divide data 
  ldaFit <- subset(lda, !is.na(lda$value)) #warnings?  
  ldaOut <- subset(lda, is.na(lda$value)) 
   
  # fit the model
  if (!is.null(var.power)){
    glmFit <- glm(value ~ factor(origin) + factor(dev), family = fam,
              data = ldaFit, offset = offset, ...)
    phi <- with(glmFit, sum(weights * residuals^2) / df.residual)
  } else{ 
    glmFit <- cpglm(value ~ factor(origin) + factor(dev),
                  link = link.power, data = ldaFit, offset = offset, ...)
    phi <- glmFit$phi
    # update fam
    fam <- tweedie(glmFit$p, link.power)
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
                
  if (mse.method == "formula"){    
    
    # process variance 
    ypv <- fam$variance(yp)
    mseProcAy <-  phi * tapply(ypv, ldaOut$origin, sum)
    mseProcTot <-  phi * sum(ypv) 
    
    # estimation variance                
    
    # get design matrix for prediction
    Terms <- delete.response(terms(glmFit))
    if (class(glmFit)[1] == "cpglm"){
      xlevels <- .getXlevels(Terms, glmFit$model.frame)
    } else { 
      xlevels <- glmFit$xlevels
    }
    X <- model.matrix(Terms, ldaOut, xlev = xlevels)
    
    # var(eta)
    v <- X %*% vcov(glmFit) %*% t(X)  # vcov has already been scaled
    muEta <- fam$mu.eta(eta)
    mseEstAy <- sapply(sort(unique(ldaOut$origin)), function(x){
  				        id <- ldaOut$origin == x	
                  # d(mu)/d(eta) from the delta method
  				        tmp <- diag(muEta[id], nrow = length(eta[id]))
  				        sum(tmp %*% v[id, id] %*% t(tmp))    				        
              })
    mseEstTot <-  sum(diag(muEta) %*% v %*% diag(muEta))
    
    # pred errs
    S.E <- sqrt(c(mseProcAy, mseProcTot) + c(mseEstAy, mseEstTot))          
  }
                       
  if (mse.method == "bootstrap"){
    nO <- nrow(ldaFit)
    nB <- length(coef(glmFit))
    # residual inflation factor
    bias <- sqrt(nO/df.residual(glmFit))
    
    resMeanAyB <- resPredAyB <- matrix(0, nsim, length(resMeanAy))    
    sims.par <- matrix(0, nsim, nB + 2)
    mu <- fitted(glmFit)
    mup <- sqrt(fam$variance(mu))
    
    # loop nsim times 
    for (i in 1:nsim){
      # while loop to avoid negative generated incremental loss
      ybad <- 1 
      while (ybad){
        rn <- bias * sample(resid(glmFit, type = "pearson"), nO, replace = TRUE)
        yB <- rn * mup + mu
        if (all(yB >= 0) || (!is.null(var.power) && var.power == 0))
          ybad <- 0  # Normal 
      }
      
      # fit model on new data
      if (!is.null(var.power)){
        glmFitB <- glm(yB ~ factor(origin) + factor(dev),
                        family = fam, data = ldaFit, offset = offset, ...)
        phi <- with(glmFitB, sum(weights * residuals^2) / df.residual)
        cf <- c(coef(glmFitB), phi, var.power)
      } else{ 
        glmFitB <- cpglm(yB ~ factor(origin) + factor(dev),
                        link = link.power, data = ldaFit, offset = offset, ...)
        cf <- c(coef(glmFitB), glmFitB$phi, glmFitB$p)
      }      
      # mean and prediction
      ymB <- predict(glmFitB, newdata = ldaOut, type = "response")
      ypB <- rtweedie(length(ymB), mu = ymB, phi = cf[nB + 1], power = cf[nB + 2])
      
      # save simulations
      resMeanAyB[i, ] <- as.numeric(tapply(ymB, ldaOut$origin, sum))      
      resPredAyB[i, ] <- as.numeric(tapply(ypB, ldaOut$origin, sum))      
      sims.par[i, ] <- unname(cf)
    }
    # get names
    dimnames(sims.par)[[2]] <- c(names(coef(glmFit)), "phi", "p")
    dimnames(resMeanAyB)[[2]] <- dimnames(resPredAyB)[[2]] <- 
            as.character(sort(unique(ldaOut$origin)))
    # pred errs
    S.E <- c(apply(resPredAyB, 2, sd), sd(rowSums(resPredAyB)))  
  }
  
  # compile results
  IBNR <- round(c(resMeanAy, resMeanTot))
  CV <- S.E / IBNR
  Latest <- getLatestCumulative(incr2cum(tr.incr))
  Latest <- Latest[-(1:(length(Latest) - length(unique(ldaOut$origin))))]
  Latest <- c(Latest, sum(Latest))
  Ultimate <- Latest + IBNR
  resDf <- data.frame(Latest = Latest, Dev.To.Date = Latest/Ultimate,
                      Ultimate = Ultimate, IBNR = IBNR,
                      S.E = S.E, CV = CV)
	row.names(resDf) <- c(as.character(sort(unique(ldaOut$origin))), "total")
  
  # produce fullly projected triangle
  ldaOut$value <- round(yp)
  FullTriangle <- as.triangle(rbind(ldaFit, ldaOut))
  if (cum) FullTriangle <- incr2cum(FullTriangle)
  
  # output
  out <- list(call = call, summary = resDf,
                Triangle = triangle,
                FullTriangle = FullTriangle,
                model = glmFit, 
                sims.par =  matrix(0),
                sims.reserve.mean =  matrix(0),
                sims.reserve.pred =  matrix(0))
  
  if (mse.method == "bootstrap"){
    out$sims.par <- sims.par 
    out$sims.reserve.mean <- resMeanAyB
    out$sims.reserve.pred <- resPredAyB
  }
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

# return pearson residuals (scaled)
residuals.glmReserve <- function(object, ...){
  m <- object$model
  r <- residuals(m, type = "pearson")
  if (class(m)[1] == "glm") {
    phi <- sum((m$weights * m$residuals^2)[m$weights > 0])/m$df.residual
  } else {
    phi <- m$phi
  }
  return(r/sqrt(phi))
}

resid.glmReserve <- function(object, ...)
  residuals(object)

# 1 Original triangle 
# 2 Full triangle 
# 3 Reserve distribution
# 4 Residual plot
# 5 QQnorm
plot.glmReserve <- function(x, which = 1, ...){
  model <- x$model
  if (which == 1){
    plot(x$Triangle, ...)  
  } else 
  if (which == 2){
    plot(x$FullTriangle, ...)  
  } else 
  if (which == 3){
    sim <- x$sims.reserve.pred
    if (ncol(sim) == 1)
      stop('No predictions found, try bootstrap?')
    sim <- cbind(sim, total = rowSums(sim))
    nm <- colnames(sim)
    sim <- data.frame(group = factor(rep(nm, rep(nrow(sim), ncol(sim))), levels = nm),
                                   prediction = as.vector(sim))
    prediction <- NULL
    gg <- ggplot(sim, aes(prediction))
    gg <- gg + geom_density() + 
      facet_wrap(~group, nrow = 2, scales = "free") +
      scale_x_continuous("predicted reserve")
    print(gg) 
  } else 
  if (which == 4){
    plot(resid(x) ~ fitted(model), ...)
    abline(h = 0, col = "#B3B3B3")
  } else  
  if (which == 5){
    qqnorm(resid(x), ...)
    qqline(resid(x))
  }   
}