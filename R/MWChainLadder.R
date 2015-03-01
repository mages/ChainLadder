##########################################################################
########  calculation of CL reserves and MSEPs
########  ****************************
########  CL_MSEPs[1:(I0+1), 1:(J0+2)]
########  i=1:I0 single accident years, I0+1 aggregated accident years
########  j=1: CL reserves
########  j=2:J0+1 (expected future) CDR MSEPs
########  j=J0+2 Mack MSEP
##########################################################################

CL_MSEPs <- function(x, I0, J0, param) {
  # Author: Mario Wuthrich
  result <- array(0, dim=c(I0+1, J0+2))       
  res <- array(0, dim=c(I0+1, 5, J0))                                                     
  for (i in (I0-J0+2):I0){
    ###### reserves and CDR MSEP formulas #########
    j <- I0-i+1
    j1 <- 1
    res[i,1,j1] <- x[i,J0]- x[i,j]                    # CL reserves of accident year i
    res[i,3,j1] <- x[i,J0]^2 * param[j,3]/x[i,j]      # CDR process variance of accident year i
    res[i,5,j1] <- param[j,3]/sum(x[(1:(I0-j)),j])  
    if ( j < (J0-1)){                               
      for (j2 in (j+1):(J0-1)){res[i,5,j1] <- res[i,5,j1] + param[j2,4]*param[j2,3]/sum(x[(1:(I0-j2)),j2])}
    }            
    res[i,4,j1] <- res[i,5,j1] * x[i,J0]^2             # CDR parameter uncertainty of accident year i                 
    res[i,2,j1] <- res[i,3,j1] + res[i,4,j1]           # CDR MSEP of accident year i
    for (i1 in (I0-J0+2):I0){                          # is needed later for aggregation
      i2 <- min(i, i1)
      res[I0+1,2,j1] <- res[I0+1,2,j1] + res[i2,5,j1] * x[i,J0] * x[i1,J0]        
    }
    
    ###### expected future reserves and future CDR MSEP formulas #########
    if ((I0-i+1)<(J0-1)) {    
      for (j in (I0-i+2):(J0-1)){
        j1 <- j-(I0-i+1)+1
        res[i,1,j1] <- x[i,J0]- x[i,j]                # expected future CL reserves of accident year i
        res[i,3,j1] <- x[i,J0]^2 * param[j,3]/x[i,j]  # expected future CDR process variance of accident year i
        y <- 1
        for (k in (1:(j1-1))){ y <- y *(1-param[j-(k-1),4])}
        res[i,5,j1] <- y * param[j,3]/sum(x[(1:(I0-j)),j])
        if ( j <(J0-1)){
          for (j2 in (j+1):(J0-1)){
            y <- 1
            for (k in (1:(j1-1))){ y <- y *(1-param[j2-(k-1),4])}
            y <- y * param[j2-(j1-1),4]
            res[i,5,j1] <- res[i,5,j1] + y * param[j2,3]/sum(x[(1:(I0-j2)),j2])
          }}            
        res[i,4,j1] <- res[i,5,j1] * x[i,J0]^2         # expected future CDR parameter uncertainty of accident year i               
        res[i,2,j1] <- res[i,3,j1]+res[i,4,j1]         # expected future CDR MSEP of accident year i
        for (i1 in (I0-J0+2):I0){                      # is needed later for aggregation
          i2 <- min(i, i1)
          res[I0+1,2,j1] <- res[I0+1,2,j1] + res[i2,5,j1] * x[i,J0] * x[i1,J0]
        }
      }
    }        
  }
  
  for (j1  in (1:J0)){                                  # aggregation over accident years
    res[I0+1,1,j1] <- sum(res[1:I0,1,j1])              # total reserves
    res[I0+1,3,j1] <- sum(res[1:I0,3,j1])              # CDR process variance aggregated accident years
    res[I0+1,4,j1] <- res[I0+1,2,j1]                   # CDR parameter uncertainty of aggregated accident years    
    res[I0+1,2,j1] <- res[I0+1,2,j1] + res[I0+1,3,j1]  # CDR MSEP aggegated accident years
  }
  ######## return results: here we could also return the whole variable "res"
  result[,1] <- res[,1,1]                                 # reserves
  for (j in (1:J0)){                                       
    result[,j+1] <- res[,2,j]                        # (expected future) CDR MSEP's
    result[,J0+2] <- result[,J0+2]+res[,2,j]         # Mack MSEP
  }
  result                 
}


CDR.MackChainLadder <- function(x, dev=1, ...){  
  # Author: Mario Wuthrich, Markus Gesmann
  if(!"MackChainLadder" %in% class(x))
    stop("The input to CDR.MackChainLadder has to be output of MackChainLadder.")
  if(!all(x$alpha==1))
    warning("The Merz & Wuthrich forumlae hold only for alpha=1.")
   
  I0 <- nrow(x$Triangle)
  J0 <- ncol(x$Triangle)
  
  C_ij <- x$FullTriangle
  sigma2 <- x$sigma^2
  ind <- seq(along=sigma2)
  f <- x$f[ind]
  ratio <- sigma2/f^2
  Latest <- rev(summary(x)[["ByOrigin"]][["Latest"]])[ind]
  alpha <-  Latest / apply(x$Triangle, 2, sum, na.rm=TRUE)[ind]
  
  CL_param <- data.frame(f, sigma2, ratio, alpha)
  
  # Check if a tail factor has been set, which means sigma tail 
  # was either set or estimated by MackChainLadder and hence 
  # nrow(CL_param) is equal to J0.
  if(nrow(CL_param) == J0){
    stop(paste0("Sorry, tail factors are currently not supported."))
  }
  CL_results <- CL_MSEPs(C_ij, I0, J0, CL_param) # Calculation MSEP's 
  CL_results[, 2:(J0+2)] <- CL_results[, 2:(J0+2)]^(1/2)
  
  CL_results <- data.frame(CL_results)
  colnames(CL_results) <- c("IBNR", paste("CDR(", c(1:J0),")S.E.", sep=""), 
                            "Mack.S.E.")
  rownames(CL_results) <- c(rownames(x$Triangle), "Total")
  
  ###################
  ### reserves, Mack S.E. and CDR S.E.
  if(("all" %in% dev) | (max(dev) > J0)){
    devs <- 1:J0
  }else{
    devs <- dev
  }
  CL_results[, c(1, 1 + devs, J0 + 2)] 
} 
