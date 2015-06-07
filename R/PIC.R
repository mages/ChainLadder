#' PaidIncurredChain
#'
#' This is the Description section, edit.
#'
#' This is the Details section, edit.
#'
#' @param triangleP Please explain
#' @param triangleI Please explain
#' @return explain what the function return
#' @details Perhaps some details here
#' @note Anything note worthy?
#' @author Fabio Concina
#' @seealso \code{\link{MackChainLadder}}
#' @references Add references
#' @examples
#' PaidIncurredChain(USAApaid, USAAincurred)
#' @export
PaidIncurredChain <- function(triangleP,triangleI) {
  # Paid Incurred Chain method (Merz, Wuthrich (2010)) in the special case
  # of non-informative priors (no prior parameter information).

  # To do list:
  # Consider a better function name, change the name in the NAMESPACE file as well
  # Consider a better output format, e.g. full triangle, s.e. for all origin periods
  # How does the user know if this model is applicable to the data at hand?
  # List model assumptions
  # Can the 'for' loops be replaced with apply statments / matrix algebra?
  # How do we know the function works? Consider tests, 
  # e.g. published examples that you can be reproduced
  # Document data sets man/USAA.Rd better
  # Add a section to the package vignette
  
  if(dim(triangleP)[1] != dim(triangleP)[2] || dim(triangleI)[1] != dim(triangleI)[2]) {
    stop("Origin and development years should be equal.")
  }
  if(dim(triangleP)[1] != dim(triangleI)[1]) {
    stop("Triangles must have same dimensions.")
  }

  J <- ncol(triangleP)
  diagP <- diag(triangleP[J:1, 1:J])[(J-1):1]

  # log(P_{i,j}/P_{i,j-1}) triangle
  fP <- matrix(data=NA, nrow=J, ncol=J)
  for (i in 1:J) {
    fP[i, 1] <- log(triangleP[i, 1])
    if (i == J) {
      break
    }
    for (j in 1:(J-i)) {
      fP[i, j+1] <- log(triangleP[i, j+1] / triangleP[i, j])
    }
  }
  fP <- as.triangle(fP)

  # log(I_{i,j} / I_{i,j+1}) triangle
  fI <- matrix(data=NA, nrow=J, ncol=J)
  for (i in 1:(J-1)) {
    for (j in 1:(J-i)) {
      fI[i, j] <- log(triangleI[i, j] / triangleI[i, j+1])
    }
  }
  fI <- as.triangle(fI)

  # sigma_j estimates, j=1,...,J-1
  sigma2.hat <- rep(NA,J)
  for (j in 1:(J-1)) {
    sigma2.hat[j] <- var(fP[,j], na.rm=T)
  }
  # sigma_J estimated through log-linear regression 
  n <- length(sigma2.hat)
  dev <- 1:n
  my.dev <- dev[!is.na(sigma2.hat) & sigma2.hat > 0]
  my.model <- lm(log(sigma2.hat[my.dev]) ~ my.dev)
  sigma2.hat[is.na(sigma2.hat)] <- exp(predict(my.model, newdata=data.frame(my.dev=dev[is.na(sigma2.hat)])))

  # tau_j estimates, j=1,...,J-2
  tau2.hat <- rep(NA,J-1)
  for (j in 1:(J-2)) {
    tau2.hat[j] <- var(fI[,j], na.rm=T)
  }
  # sigma_{J-1} estimated through log-linear regression
  n <- length(tau2.hat)
  dev <- 1:n
  my.dev <- dev[!is.na(tau2.hat) & tau2.hat > 0]
  my.model <- lm(log(tau2.hat[my.dev]) ~ my.dev)
  tau2.hat[is.na(tau2.hat)] <- exp(predict(my.model, newdata=data.frame(my.dev=dev[is.na(tau2.hat)])))

  # v2_j estimates, j=1,...,J
  v2 <- rep(NA,J)
  for (i in 1:(J-1)) {
    v2[i] <- sum(sigma2.hat) + sum(tau2.hat[i:J-1])
  }
  v2[J] <- sum(sigma2.hat)

  # w2_j estimates, j=1,...,J
  w2 <- rep(NA,J)
  for (i in 1:J) {
    w2[i] <- sum(sigma2.hat[1:i])
  }

  # (c_1,...,c_J,b_1,...,b_{J-1}) parameters
  c <- c()
  for (j in 1:J)	{
    if (j==1) {
      c[j] <- (1/sigma2.hat[j]) * sum(fP[1:J,j])
    } 
    else if (j==2) {
      c[j] <- (1/sigma2.hat[j]) * sum(fP[1:(J+1-j),j]) +
      sum( 1/(v2[1:(j-1)]-w2[1:(j-1)]) *
      log(triangleI[J:(J-j+2),1:(j-1)]/triangleP[J:(J-j+2),1:(j-1)]))
    } else {
        diag2 <- diag(triangleI[J:(J-j+2),1:(j-1)])
        diag <- diag(triangleP[J:(J-j+2),1:(j-1)])
        c[j] <- (1/sigma2.hat[j]) * sum(fP[1:(J+1-j),j]) +
        sum( 1/(v2[1:(j-1)]-w2[1:(j-1)]) *
        log(diag2[1:(j-1)]/diag[1:(j-1)]))
    }
  }
    
  b <- c()
  for (j in 1:(J-1))  {
    if (j==1) {
      b[j] <- -(1/tau2.hat[j]) * sum(fI[1:(J-j),j]) -
      sum( 1/(v2[1:j]-w2[1:j]) *
      log(triangleI[J:(J-j+1),1:j]/triangleP[J:(J-j+1),1:j]))
    } else {
      diag2 <- diag(triangleI[J:(J-j+1),1:j])
      diag <- diag(triangleP[J:(J-j+1),1:j])
      b[j] <- -(1/tau2.hat[j]) * sum(fI[1:(J-j),j]) -
      sum( 1/(v2[1:j]-w2[1:j]) *
      log(diag2[1:j]/diag[1:j]))
    }
  }

  # inverse covariance matrix
  A <- matrix(NA, nrow=(J + J - 1), ncol=(J + J - 1))
  for (n in 0:(J-1)) {
    for (m in 0:(J-1)) {
      if(n==m) {
        if (n==0) {
          A[n+1,m+1] <- (J - n)/sigma2.hat[n+1]	
        } else {
          A[n+1,m+1] <- ((J - 1) - n + 1)/sigma2.hat[n+1] +
          sum(1/(v2[1:(min(n-1,m-1)+1)] -
          w2[1:(min(n-1,m-1)+1)]))
        }
      } else {
        if (n==0 | m==0) {
          A[n+1,m+1] <- 0	
        } else {
          A[n+1,m+1] <- sum(1/(v2[1:(min(n-1,m-1)+1)] -
          w2[1:(min(n-1,m-1)+1)]))	
        }
      }		
    }
  }

  for (n in 0:(J-2)) {
    for (m in 0:(J-2)) {
      if(n==m) {
        A[J+n+1,J+m+1] <- (J-n-1)/tau2.hat[n+1] +
        sum(1/(v2[1:min(n+1,m+1)] -
        w2[1:min(n+1,m+1)]))
      } else {
	    A[J+n+1,J+m+1] <- sum(1/(v2[1:min(n+1,m+1)] - w2[1:min(n+1,m+1)]))	
      }
    }
  }

  for (n in 0:(J-1)) {
    for (m in 0:(J-2)) {	
      if (n==0 | m==0) {
        A[n+1,J+m+1] <- 0	
      } else {
        A[n+1,J+m+1] <- -sum(1/(v2[1:(min(n-1,m)+1)] -
        w2[1:(min(n-1,m)+1)]))	
      }				
    }
  }

  for (n in 0:(J-2)) {
    for (m in 0:(J-1)) {	
      if (n==0 | m==0) {
        A[J+n+1,m+1] <- 0	
      } else {
        A[J+n+1,m+1] <- -sum(1/(v2[1:(min(n,m-1)+1)] -
        w2[1:(min(n,m-1)+1)]))	
      }
    }
  }
  
  # the inverse of the inverse is the posterior covariance matrix 
  Ainv <- solve(A)

  # posterior parameters
  cb <- c(c,b)
  theta.post <- Ainv %*% cb

  # beta and s2.post parameters
  beta <- c()
  for (i in 1:(J-1)) {
    beta[i] <- (v2[J] - w2[i])/(v2[i] - w2[i]) 
  }
  
  s2.post <- c()
  E <- matrix(NA,nrow=(J-1),ncol=(2*J-1))
  for (i in 2:J) {
    e <- rep(0,J+1-i)
    e <- c(e,rep(1 - beta[J+1-i],i-1))
    e <- c(e,rep(0,J-i))
    e <- c(e,rep(beta[J+1-i],i-1))
    E[i-1,] <- e
    s2.post[i-1] <- e %*% Ainv %*% e
  }
  
  # ultimate loss vector
  PIC.ult <- c()
  for (i in 2:J) {
    PIC.ult[i-1] <- triangleP[i,J+1-i]^(1 - beta[J+1-i]) *
    triangleI[i,J+1-i]^(beta[J+1-i]) * exp((1 - beta[J+1-i]) *
    sum(theta.post[(J-i+2):J]) + beta[J+1-i] *
    sum(theta.post[(2*J-i+1):(2*J-1)])) * exp((1 - beta[J+1-i]) *
    (v2[J] - w2[J-i+1])/2 + s2.post[i-1]/2)
  }

  # claims reserves
  PIC.Ris <- PIC.ult - diagP
  PIC.RisTot <- sum(PIC.Ris)

  # prediction uncertainty
  msep <- 0
  for (i in 2:J) {
    for (k in 2:J) {
      if (i==k) {
        msep <- msep + (exp((1-beta[J+1-i]) * (v2[J] - w2[J+1-i]) +
        E[i-1, ]%*% Ainv %*% E[k-1, ]) - 1) * PIC.ult[i-1] * PIC.ult[k-1]
      } else {
        msep <- msep + (exp(E[i-1, ] %*% Ainv %*% E[k-1, ]) - 1) *
        PIC.ult[i-1] * PIC.ult[k-1]	
      }
    }
  }
  PIC.se <- sqrt(msep)

  return(list(Res.Origin=PIC.Ris,Res.Tot=PIC.RisTot,s.e.=PIC.se))
}
