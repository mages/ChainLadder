#################################################
###                                           ###
###     Tweedie Stochastic Reserving Model    ###
###             (glmReserve mod)              ###
###           by Alessandro Carrato           ###
###       alessandro.carrato@gmail.com        ###
###                                           ###
#################################################

####ONLY FOR DEBUG####
########START#########

#Load toList function (to convert data.set in matrix ... to be improved?
toList <- function(data) {
  matr<-cbind(data[,1],data[,2])
  for(i in 3:length(data[1,])){
    matr<-cbind(matr,data[,i])
  }
  matr
}

####ONLY FOR DEBUG####
#########END##########

fit_gamma <- function(coeffs,design.type,n){
  pos.gamma<-2 #intercept + next position = 2
  for (i in 1:2){ 
    if (design.type[i] == 1) {
      pos.gamma<-pos.gamma+n
    }
    if (design.type[i] == 2) {
      pos.gamma<-pos.gamma+1
    }
  }
  y<-coeffs[pos.gamma:length(coeffs)]
  y<-y[!is.na(y)]
  x<-seq(1,length(y))
  new <- data.frame(x = seq(1, 2*n))
  new$y<-predict(lm(y~x),new)
  gamma_y<-rep(0,2*n)
  for (k in 1:(2*n)){
    if (!is.na(coeffs[pos.gamma+k-1])){
      gamma_y[k]<-coeffs[pos.gamma+k-1]
    } else {
      gamma_y[k]<-new$y[k]
    }    
  }
  
  out<-c(list(coeffs=append(coeffs[1:(pos.gamma-1)],gamma_y),gamma_y=gamma_y))
  return(out)
}


tweedieReserve <- function(triangle, var.power=1, link.power=0, 
                           design.type=c(1,1,0), rereserving=FALSE,##link.power=0 is the log link ...
                           cum=TRUE, exposure=FALSE, bootstrap=1, 
                           boot.adj=0, nsim=1000, proc.err=TRUE, 
                           p.optim=FALSE, p.check=c(0,seq(1.1,2.1,by=0.1),3),
                           progressBar=TRUE,...){
  
  
  ## The following variable will be generated later 'on the fly'
  ## To avoid NOTE from R CMD CHECK let's set them to NULL first.
  glmFit <- NULL
  glmFitB <- NULL
  glmFit1yr <- NULL
  
  call <- match.call()
  if (!("triangle") %in% class(triangle))
    stop("triangle must be of class 'triangle'")
  if ("offset" %in% names(list(...)))
    stop("'offset' should be passed using the 
         'exposure' attribute of the triangle!")
  if ("weigth" %in% names(list(...)))
    stop("'weight' should not be used")  
  # convert to incremental losses if needed
  # tr.incr <- cum2incr(triangleC)   ## giusto per far prima in debug!!
  tr.incr <- if (cum) cum2incr(triangle) else triangle
  
  # create family                            
  family <- tweedie(var.power=var.power, link.power=link.power)
  
  # convert to long format
  lda <-  as.data.frame(tr.incr)
  lda$offset <- if (is.null(attr(tr.incr,"exposure")))
    rep(0,nrow(lda)) else 
      family$linkfun(attr(tr.incr,"exposure")[lda$origin])
  
  #parameter fix for better intrepretation of results
  base.year <- min(lda$origin)
  lda$origin <- lda$origin - base.year + 1 # ORIGIN MUST START FROM 1
  
  base.dev <- min(lda$dev)
  lda$dev <- lda$dev - base.dev  # DEV MUST START FROM 1
  lda$cy <- lda$origin + lda$dev
  
  ######################################
  ####DESIGN MATRIX CALCULATION#####
  ######################################
  
  
  #  design matrix per AY
  design.string <- " "
  m_dev <- max(lda["dev"]) ##max_dev (IT MUST START from 0!!!)
  m_cy <- 2*m_dev
  m <- nrow(lda)
  
  temp <- rep(1,m)
  
  if (design.type[1] == 0){
    ay<-matrix(0,nrow=m,ncol=1)
  }
  if (design.type[1] == 1){
    
    design.string <- paste(design.string,"factor(origin)")
    
    ay <- matrix(0, nrow=m, ncol=m_dev)
    for(r in 1:m){
      if (lda$origin[r]!=1) {
        ay[r,lda$origin[r]-1] <- 1
      }
    }
    ay <- cbind(temp,ay)       
  }
  if (design.type[1] == 2){
    
    design.string <- paste(design.string, "origin")
    
    ay <- matrix(0,nrow=m, ncol=1)
    for(r in 1:m){
      ay[r, 1] <- lda$origin[r]
    }
    ay<-cbind(temp, ay)
  }
  
  #  design matrix per DY
  if (design.type[2] == 0){
    dy <- matrix(0, nrow=m, ncol=1)
  }
  if (design.type[2] == 1){
    
    if (design.type[1]==0) {
      design.string <- paste(design.string, "factor(dev)")
    } else {
      design.string <- paste(design.string, "+ factor(dev)")
    }
    
    dy <- matrix(0,nrow=m, ncol=m_dev)
    for(r in 1:m){
      if (lda$dev[r]!=0) { 
        dy[r,lda$dev[r]] <- 1 
      }
    }
    dy <- cbind(temp, dy)
    
  }
  if (design.type[2] == 2){
    
    if (design.type[1]==0) {
      design.string <- paste(design.string, "dev") 
    } else {
      design.string <- paste(design.string,"+ dev")
    }
    
    dy<-matrix(0,nrow=m, ncol=1)
    for(r in 1:m){
      dy[r, 1] <- lda$dev[r]
    }
    dy<-cbind(temp,dy)
  }
  
  #  design matrix per CY
  if (design.type[3] == 0){
    cy <- matrix(0, nrow=m, ncol=1)
  }
  if (design.type[3] == 1){
    
    if (design.type[1]==0 && design.type[2]==0) {
      design.string <- paste(design.string, "factor(cy)")
    } else {
      design.string <- paste(design.string, "+ factor(cy)")
    }
    
    cy <- matrix(0,nrow=m, ncol=2*m_dev)
    for(r in 1:m) {
      if (lda$cy[r]!=1) {
        cy[r,lda$cy[r]-1] <- 1
      }
    }
    cy <- cbind(temp,cy)
    
  }
  if (design.type[3] == 2){
    
    if (design.type[1]==0 && design.type[2]==0) {
      design.string <- paste(design.string, "cy")
    } else {
      design.string <- paste(design.string, "+ cy")
    }
    
    cy <- matrix(0, nrow=m, ncol=1)
    for(r in 1:m){
      cy[r, 1] <- lda$cy[r]
    }
    cy <- cbind(temp, cy)
    
  }
  
  design.matrix <- cbind(ay, dy, cy)
  design.matrix <- cbind(temp, subset(design.matrix, 
                                   select=-c(1, length(ay[1,]) + 1,
                                             (length(ay[1,]) + length(dy[1,])+1))
                                   )
                         )
  
  ######################################
  ####END DESIGN MATRIX CALCULATION#####
  ######################################
  
  
  ldaFit <- subset(lda, !is.na(lda$value)) 
  ldaOut <- subset(lda, is.na(lda$value))
  
  # fit the model
  #glmFit<-glm(value~factor(origin)+factor(dev),family=quasipoisson(log),data=ldaFit)
  
  if (exposure){
    glmstring <- paste(design.string,", family = family, data=ldaFit,offset=offset")
  } else {
    glmstring <- paste(design.string,", family = family, data=ldaFit")
  }
  
  eval(parse(text=
               c(
                 "glmFit<-glm(value ~", glmstring,
                 ")"
               )
  )
  )  
  
  ################################
  ## calculate reserve 
  ################################
  
  # prediction for each cell
  
  coeffs <- glmFit$coefficients
  temp_y <- data.frame(gamma_y<-c(0))
  
  # an extrapultion of future CY factors has to be made for the future CY
  if (design.type[3]==1) {
    temp_y <- fit_gamma(coeffs, design.type, n=m_dev) ## NOTE: 0 included!!! so actually we have (n+1) values!!
    coeffs <- temp_y$coeffs
  }
  n <- nrow(ldaFit) ## number of data points (n)
  d.f <- df.residual(glmFit) ## number of data points - parameters (n-p)
  bias <- sqrt(n/d.f)
  
  # dispersion
  #phi <- sum(resid(glmFit,type="pearson")^2)/d.f
  phi <- summary(glmFit)$dispersion
  
  lda$eta <- design.matrix %*% coeffs
  if(exposure) {
    lda$eta <- lda$eta + lda$offset
  }
  lda$yp <- exp(lda$eta)  
  
  # sum to get reserve by year
  
  resMeanAy <- tapply(lda$yp[is.na(lda$value)], ldaOut$origin, sum)
  resMeanTot <- sum(resMeanAy)       
  
  Reserve <- round(c(resMeanAy, resMeanTot))
  Latest <- getLatestCumulative(incr2cum(tr.incr))[-1L]
  Latest <- c(Latest, sum(Latest))
  Ultimate <- Latest + Reserve
  
  
  count.neg<-0
  ###BOOTSTRAP CYCLE###
  if (bootstrap!=0){
    ## This only works on Windows
    ## pb <- winProgressBar(title = "progress bar", min = 1, max = nsim, width = 300)
    if(progressBar){
      pb <- txtProgressBar(min = 1, max = nsim, style=3)
    }
    resMeanAyB <- matrix(0, length(resMeanAy), nsim)
    resMeanTotB <- rep(0, nsim)
    
    if (rereserving) { ##CREATE 1yr matrix
      resMeanAyB_1yr <- matrix(0, length(resMeanAy), nsim)
      resMeanTotB_1yr <- rep(0, nsim)
      
    }
    
    # loop nsim times
    for (b in 1:nsim){      
      mu <- lda$yp[!is.na(lda$value)]
      
      ### PARAMETRIC BOOTSTRAP ###
      if(bootstrap==1) {
        if(var.power!=0){ ### rtweedie doesn't support var.power = 0
          yB=rtweedie(n, phi=phi, xi=var.power, mu=mu) 
        } else{
          yB=rnorm(n, mean=mu, sd=sqrt(phi)) #### KNOWN ISSUE: Error in glm.fit with negative values .. why?
        }
      } else { ### SEMI-PARAMETRIC BOOTSTRAP ###      
        if(boot.adj==0){  ### WHILE CYCLE until triangle > 0 ... WARNING: COULD BE TIME CONSUMING!!!!
          ybad <- 1 
          tries=0
          while (ybad){
            tries = tries + 1
            if (tries>100) {
              stop("Too many negative bootstrapped values!!")
              } ### Added to exit code, otherwise "infinite" WHILE cycle could happen!!
            
            rn <- bias * sample(resid(glmFit, type = "pearson"), n, replace = TRUE)
            yB <- rn * sqrt(family$variance(mu)) + mu
            
            if (all(yB >= 0) || (!is.null(var.power) && var.power == 0)) #### KNOWN ISSUE: Error in glm.fit with negative values .. why?
              ybad <- 0  # Normal 
          }
          
        } else{ ### OVERWRITE NEGATIVE VALUES with 0.01 ... WARNING: COULD LEAD TO LOWER UNCERTAINTY!!
          rn <- bias * sample(resid(glmFit,type="pearson"), n, replace=TRUE) ##adjustment for df
          yB <- rn * sqrt(family$variance(mu)) + mu      
          if (!all(yB>=0) || (!is.null(var.power) && var.power != 0)) {
            for (i in 1:n){
              if (yB[i] <= 0) {        
                yB[i] <- 0.01
                count.neg <- count.neg + 1
              }
            }
          }
        }
      }
      
      
      eval(parse(text=
                   c(
                     "glmFitB<-glm(yB~",glmstring,
                     ")"
                   )
      )
      )
      
      coeffsB <- glmFitB$coefficients
      
      if (design.type[3]==1) {
        temp_yB <- fit_gamma(coeffsB, design.type, n=m_dev) ## NOTE: DEV must start with 0!!!
        coeffsB <- temp_yB$coeffs
      }
      
      lda$etaB <- design.matrix %*% coeffsB
      if(exposure) {
        lda$etaB <- lda$etaB + lda$offset
      }
      lda$ypB <- exp(lda$etaB)
      
      ##ADD PROCESS ERROR
      if (proc.err){
        n.fut <- length(lda$ypB[is.na(lda$value)])
        mu.fut <- lda$ypB[is.na(lda$value)]
        if (var.power!=0){
          lda$ypB[is.na(lda$value)] <- rtweedie(n.fut, xi=var.power,
                                                mu=mu.fut, phi=phi)  
        } else {
          lda$ypB[is.na(lda$value)] <- rnorm(n.fut, mean=mu.fut, sd=sqrt(phi))
        }
      }
      
      
      resMeanAyB[,b] <- tapply(lda$ypB[is.na(lda$value)], ldaOut$origin, sum)
      resMeanTotB[b] <- sum(resMeanAyB[,b])
      
      ### 1yr VIEW Calculation ###
      if (rereserving) {
        
        lda$year1 <- lda$value  ## COPY ALL THE KNOWN VALUES
        next_y <- max(lda$origin)+1 ##NEXT DIAGONAL is for CY = max(origin)+1
        lda$year1[lda$cy==next_y]=lda$ypB[lda$cy==next_y]  ##COPY NEXT DIAGONAL
        ldaFit_1yr <- subset(lda,!is.na(lda$year1))
        
        if (exposure){
          glmstring_1yr <- paste(design.string,", family = family, data=ldaFit_1yr,offset=offset")
        } else{
          glmstring_1yr <- paste(design.string,", family = family, data=ldaFit_1yr")
        }
        
        eval(parse(text=
                     c(
                       "glmFit1yr<-glm(year1~",glmstring_1yr,
                       ")"
                     )
        )
        )
        
        coeffs1yr <- glmFit1yr$coefficients
        
        if (design.type[3]==1) {
          temp_1yr <- fit_gamma(coeffs1yr,design.type,n=m_dev) ## NOTE: 0 included!!! so actually we have (n+1) values!!
          coeffs1yr <- temp_1yr$coeffs
        }
        
        lda$be1yr <- design.matrix %*% coeffs1yr ##ETA
        if(exposure) {
          lda$be1yr <- lda$be1yr + lda$offset
          }        ##ETA + OFFSET
        lda$be1yr <- exp(lda$be1yr)              ##Best Estimate 1yr, for all the values ..
        
        lda$year1[is.na(lda$year1)] <- lda$be1yr[is.na(lda$year1)]
        
        resMeanAyB_1yr[,b] <- tapply(lda$year1[is.na(lda$value)],
                                     ldaOut$origin, sum)
        resMeanTotB_1yr[b] <- sum(resMeanAyB_1yr[,b])        
      }
      ##setWinProgressBar(pb, b, title=paste(round(b/nsim*100, 0),"% Done"))
      if(progressBar)
        setTxtProgressBar(pb, b)
      
    } ## end nsim look
    
    ##the adjustment for DF is included directly in residuals (see England(2002) Addendum) or in parametric bootstrap
    mseEstAy <- apply(resMeanAyB, 1, var)
    mseEstTot <- var(resMeanTotB)
    
    avgResAy <- apply(resMeanAyB, 1, mean)
    avgResTot <- mean(resMeanTotB)
    
    Expected.Reserve <- round(c(avgResAy, avgResTot))
    
    S.E <- sqrt(c(mseEstAy, mseEstTot))
    CoV <- S.E / Expected.Reserve
    
    if (rereserving) {
      mseEstAy_1yr <- apply(resMeanAyB_1yr, 1, var)
      mseEstTot_1yr <- var(resMeanTotB_1yr)
      
      avgResAy_1yr <- apply(resMeanAyB_1yr, 1, mean)
      avgResTot_1yr <- mean(resMeanTotB_1yr)
      
      Expected.Reserve_1yr <- round(c(avgResAy_1yr, avgResTot_1yr))
      
      S.E_1yr <- sqrt(c(mseEstAy_1yr, mseEstTot_1yr))
      CoV_1yr <- S.E_1yr / Expected.Reserve_1yr
      
      Emergence.Pattern <- S.E_1yr/S.E
    }
  }## end bootstrap
  
  # percentage of negative values modified
  
  perc.neg<-count.neg/(nsim*n)
  
  
  #WRITING REPORT
  if (bootstrap!=0) {
    if (rereserving) {
      resDf <- data.frame(Latest=Latest, 
                          IBNR=Expected.Reserve,
                          IBNR.S.E=S.E,
                          CoV=CoV,
                          Ultimate=Latest+Expected.Reserve,
                          Det.IBNR=Reserve,
                          CDR=Expected.Reserve_1yr,
                          CDR.S.E=S.E_1yr,
                          Emergence.Pattern = Emergence.Pattern,
                          Dev.To.Date=Latest/Ultimate)                     
    } else {
      resDf <- data.frame(Latest=Latest, 
                          IBNR=Expected.Reserve,
                          IBNR.S.E=S.E,
                          CoV=CoV,
                          Ultimate=Latest+Expected.Reserve,
                          Det.IBNR=Reserve,
                          Dev.To.Date=Latest/Ultimate)
    }
    
  } else {
    resDf <- data.frame(Latest=Latest, 
                        Det.IBNR=Reserve,
                        Ultimate=Ultimate,
                        Dev.To.Date=Latest/Ultimate)
  }
  
  row.names(resDf) <- c(as.character(sort(unique(ldaOut$origin))),"total")
  
  # produce fully projected triangle
  ldaOut$value <- round(lda$yp[is.na(lda$value)])
  FullTriangle <- as.triangle(rbind(ldaFit,ldaOut))
  if (cum)
    FullTriangle <- incr2cum(FullTriangle)
  
  res.diag<-data.frame(unscaled=resid(glmFit,type="pearson"),
                       unscaled.biasadj=resid(glmFit,type="pearson")*bias,
                       scaled=resid(glmFit,type="pearson")/sqrt(phi),
                       scaled.biasadj=resid(glmFit,type="pearson")*bias/sqrt(phi),
                       dev=ldaFit$dev,
                       origin=ldaFit$origin,
                       cy=ldaFit$cy)
  
  # output
  out<-list(call=call,
            summary=resDf,
            Triangle=triangle,
            FullTriangle=FullTriangle,
            scale=phi,
            bias=bias,
            GLMReserve=resMeanTot,
            gamma_y=temp_y$gamma_y,
            res.diag=res.diag,
            rereserving=rereserving,
            bootstrap=bootstrap)
  
  if (bootstrap != 0) {
    if (rereserving) {
      out<-c(out,list(distr.res_ult=resMeanTotB,
                      distr.res_1yr=resMeanTotB_1yr
      ))
    } else {
      out<-c(out,list(distr.res_ult=resMeanTotB))  
    }
  }
  
  out<-c(out,glmFit[!(names(glmFit) %in% c("call"))])
  
  if (p.optim){
  
    if (exposure){
      glmstring<-paste(design.string,", p.vec=p.check, data=ldaFit, do.plot=TRUE, offset=offset")
    } else {
      glmstring<-paste(design.string,", p.vec=p.check, data=ldaFit, do.plot=TRUE")
    }
    eval(parse(text=
                 c(
                   "p<-tweedie.profile(value ~", glmstring,
                   ")"
                 )
    )
    ) 
    
  }
  
  class(out) <- c("tweedieReserve", "glm")
  return(out)  
}

print.tweedieReserve <- function(x,...){
  print(x$call)
  cat("\n")
  if (x$bootstrap != 0) {
    if (x$rereserving) {     
      out <- x$summary[c("Latest", "IBNR", "IBNR.S.E", "CDR.S.E")]
      names(out)[4] <- "CDR(1)S.E"
    }
    else{    
      out <- x$summary[c("Latest", "IBNR", "IBNR.S.E")]
    }
  }
  else{
    out <- x$summary[c("Latest", "Det.IBNR", "Ultimate")]
  }
  print(out)  
}

summary.tweedieReserve <- function(object,
                                   q=c(0.5,0.75,0.9,0.95,0.995),...){
  #if (class(res) != "stochasticReserving")
  #  stop("res must be of class 'stochasticReserving'")
  res <- object
  if(res$rereserving){
    out<- list(    
      Prediction=data.frame(
        IBNR=c(mean(res$distr.res_ult),
                        sd(res$distr.res_ult),
                        #sd(res$distr.res_ult)/mean(res$distr.res_ult),
                        quantile(res$distr.res_ult,q)
        ),
        'CDR(1)'=c(mean(res$distr.res_1yr),
                     sd(res$distr.res_1yr),
                     #sd(res$distr.res_1yr)/mean(res$distr.res_1yr),
                     quantile(res$distr.res_1yr,q)
        )
      ),
      Diagnostic=c(GLMReserve=res$GLMReserve,
                   "mean(IBNR)"=mean(res$distr.res_ult),
                   "mean(CDR(1))"=mean(res$distr.res_1yr))
    )
    names(out$Prediction)[2]="CDR(1)"
  }else{
    out<- list(    
      Reserves=data.frame(
        IBNR=c(mean(res$distr.res_ult),
                        sd(res$distr.res_ult),
                        #sd(res$distr.res_ult)/mean(res$distr.res_ult),
                        quantile(res$distr.res_ult,q)
        )
      ),
      Diagnostic=c(GLMReserve=res$GLMReserve,
                   "mean(IBNR)"=mean(res$distr.res_ult))
    )
  }
  
  rownames(out$Prediction) <- c("mean", "sd", paste0(q*100, "%"))          
  print(out)
}