###################################################
##    glm-based insurance loss reserving 
##            T. Moudiki
##     thierry.moudiki@gmail.com
###################################################

mlReserve <- function(triangle, var.power = 1, link.power = 0,
                cum = TRUE, 
                nsim = 1000, nb = FALSE, 
                fit_func = stats::glm,
                predict_func = predict,
                ...){

  origin <- NULL
  call <- match.call()
  
  if (!("triangle") %in% class(triangle))
    stop("triangle must be of class 'triangle'")
  if ("offset" %in% names(list(...)))
    stop("'offset' should be passed using the 
         'exposure' attribute of the triangle!")
  if ("weight" %in% names(list(...)))
    stop("'weight' should not be used")  
  
  # convert to incremental losses if needed    
  tr.incr <- if (cum) cum2incr(triangle) else triangle    
  
  # convert to long format
  lda <-  as.data.frame(tr.incr, origin = names(dimnames(tr.incr))[1], 
                        dev = names(dimnames(tr.incr))[2])
  names(lda)[1:3] <- c("origin", "dev", "value")
  lda <- transform(lda, origin = factor(origin, levels = dimnames(triangle)[[1]]))
  
  # create offset
  if (is.null(attr(tr.incr, "exposure"))) {
    lda$offset <-  rep(0, nrow(lda))
  } else {
    # Allow exposures to be expanded to the long data.frame by matching
    #   names(exposure) with triangle's origin values, or by the arithmetic
    #   formula that has always been there, if triangles origin values are
    #   convertible from their character representations to numeric.
    expo <- attr(tr.incr, "exposure")
    if (is.null(names(expo))) names(expo) <- NA
    if (all(names(expo) %in% lda$origin)) lda$offset <- 
        fam$linkfun(attr(tr.incr, "exposure")[lda$origin])
    else {
      numorig <- suppressWarnings(as.numeric(lda$origin))
      if (any(is.na(numorig))) stop(
        "Unnamed exposures incompatible when triangle's origin values are not convertible from character to numeric."
      )
      lda$offset <- 
        fam$linkfun(attr(tr.incr, "exposure")[numorig - min (numorig) + 1])
    }
  }
  
  # divide data 
  ldaFit <- subset(lda, !is.na(lda$value)) #warnings?  
  ldaOut <- subset(lda, is.na(lda$value)) 
   
  # fit the model
  ldaFit$value <- round(ldaFit$value)  #warning
  mlFit <- try(fit_func(value ~ factor(origin) + factor(dev), 
                       data = ldaFit, ...))
  misc::debug_print(mlFit)
  misc::debug_print(ldaOut)
  yp <- predict_func(mlFit, ldaOut[, c("origin", "dev")])
  misc::debug_print(yp)
  ################################
  ## calculate reserves 
  ################################
  yp <- as.numeric(yp)
  if (length(yp) != nrow(ldaOut)) {
    stop("Prediction function returned incorrect number of predictions. ",
         "Expected ", nrow(ldaOut), " got ", length(yp))
  }
  
  eta <- yp  # store predictions before summing
  # sum to get reserve by year
  resMeanAy <- tapply(yp, factor(ldaOut$origin), sum)
  misc::debug_print(resMeanAy)                
  resMeanTot <- sum(resMeanAy)
  misc::debug_print(resMeanTot)                

  ################################
  ## calculate prediction errs 
  ################################                
                       
  # compile results
  IBNR <- round(c(resMeanAy, resMeanTot))
  resids <- eta - yp
  S.E <- sqrt(sum(resids^2) / (nrow(ldaOut) - 2))
  CV <- S.E / IBNR
  Latest <- getLatestCumulative(incr2cum(tr.incr))
  Latest <- Latest[-(1:(length(Latest) - length(unique(ldaOut$origin))))]
  Latest <- c(Latest, total = sum(Latest))
  Ultimate <- Latest + IBNR
  resDf <- data.frame(Latest = Latest, Dev.To.Date = Latest/Ultimate,
                      Ultimate = Ultimate, IBNR = IBNR,
                      S.E = S.E, CV = CV)
	row.names(resDf) <- names(Latest)
  
  # produce fullly projected triangle
  ldaOut$value <- round(yp)
  FullTriangle <- as.triangle(rbind(ldaFit, ldaOut))
  if (cum) FullTriangle <- incr2cum(FullTriangle)
  
  # output
  out <- list(call = call, summary = resDf,
                Triangle = triangle,
                FullTriangle = FullTriangle,
                model = mlFit, 
                sims.par =  matrix(0),
                sims.reserve.mean =  matrix(0),
                sims.reserve.pred =  matrix(0))
  
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

print.glmReserve <- function(x, ...) {
  summary(x)
  
  invisible(x)
}

# return pearson residuals (scaled)
residuals.glmReserve <- function(object, ...){
  m <- object$model
  r <- residuals(m, type = "pearson")
  if (class(m)[1] == "glm") {
    phi <- sum((m$weights * m$residuals^2)[m$weights > 0])/m$df.residual
  } else if (class(m)[1] == "glm"){
    phi <- 1
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