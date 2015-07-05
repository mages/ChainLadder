#' PaidIncurredChain
#'
#' The Paid-incurred Chain model (Merz, Wuthrich (2010)) combines 
#' claims payments and incurred losses information to get 
#' a unified ultimate loss prediction.
#' 
#' The method uses some basic properties of multivariate Gaussian distributions
#' to obtain a mathematically rigorous and consistent model for the combination
#' of the two information channels.
#' 
#' @param triangleP Cumulative claims payments triangle
#' @param triangleI Incurred losses triangle.
#' @return The function returns:
#' \itemize{
#'   \item \strong{Ult.Loss.Origin} Ultimate losses for different origin years.
#'   \item \strong{Ult.Loss} Total ultimate loss.
#'   \item \strong{Res.Origin} Claims reserves for different origin years.
#'   \item \strong{Res.Tot} Total reserve.
#'   \item \strong{s.e.} Square root of mean square error of prediction 
#'   for the total ultimate loss.
#' }
#' @details
#' We assume as usual that I=J. 
#' The model assumptions for the Log-Normal PIC Model are the following:
#' \itemize{
#'   \item Conditionally, given \eqn{latex}{\Theta = (\Phi_0,...,\Phi_I,
#'   \Psi_0,...,\Psi_{I-1},\sigma_0,...,\sigma_{I-1},\tau_0,...,\tau_{I-1})}
#'   we have
#'   \itemize{
#'     \item the random vector \eqn{latex}{(\xi_{0,0},...,\xi_{I,I},
#'     \zeta_{0,0},...,\zeta_{I,I-1})} has multivariate Gaussian distribution
#'     with uncorrelated components given by
#'     \deqn{latex}{\xi_{i,j} \sim N(\Phi_j,\sigma^2_j),}
#'     \deqn{latex}{\zeta_{k,l} \sim N(\Psi_l,\tau^2_l);}
#'     \item cumulative payments are given by the recursion
#'     \deqn{latex}{P_{i,j} = P_{i,j-1} \exp(\xi_{i,j}),}
#'     with initial value \eqn{P_{i,0} = \exp (\xi_{i,0})};
#'     \item incurred losses \eqn{I_{i,j}} are given by the backwards
#'     recursion
#'     \deqn{latex}{I_{i,j-1} = I_{i,j} \exp(-\zeta_{i,j-1}),}
#'     with initial value \eqn{I_{i,I}=P_{i,I}}.
#'   }
#'   \item The components of \eqn{latex}{\Theta} are indipendent and 
#'   \eqn{latex}{\sigma_j,\tau_j > 0} for all j.
#'  }
#'  
#' 
#' Parameters \eqn{latex}{\Theta} in the model are in general not known and need to be
#' estimated from observations. They are estimated in a Bayesian framework.
#' In the Bayesian PIC model they assume that the previous assumptions 
#' hold true with deterministic \eqn{latex}{\sigma_0,...,\sigma_J} and 
#' \eqn{latex}{\tau_0,...,\tau_{J-1}} and
#' \deqn{latex}{\Phi_m \sim N(\phi_m,s^2_m),}
#' \deqn{latex}{\Psi_n \sim N(\psi_n,t^2_n).}
#' This is not a full Bayesian approach but has the advantage to give
#' analytical expressions for the posterior distributions and the prediction
#' uncertainty.
#' 
#' @note The model is implemented in the special case of non-informative priors.
#' @author Fabio Concina, \email{fabio.concina@@gmail.com}
#' @seealso \code{\link{MackChainLadder}},\code{\link{MunichChainLadder}}
#' @references Merz, M., Wuthrich, M. (2010). Paid-incurred chain claims reserving method. 
#' Insurance: Mathematics and Economics, 46(3), 568-579.
#' @examples
#' PaidIncurredChain(USAApaid, USAAincurred)
#' @export
PaidIncurredChain <- function(triangleP,triangleI) {
  
  # To do list:
  # Consider a better function name, change the name in the NAMESPACE file as well
  # Consider a better output format, e.g. full triangle, s.e. for all origin periods
  # How does the user know if this model is applicable to the data at hand?
  # Can the 'for' loops be replaced with apply statments / matrix algebra?
  # How do we know the function works? Consider tests, 
  # e.g. published examples that you can be reproduced
  
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
  PIC.Ult <- c()
  for (i in 2:J) {
    PIC.Ult[i-1] <- triangleP[i,J+1-i]^(1 - beta[J+1-i]) *
    triangleI[i,J+1-i]^(beta[J+1-i]) * exp((1 - beta[J+1-i]) *
    sum(theta.post[(J-i+2):J]) + beta[J+1-i] *
    sum(theta.post[(2*J-i+1):(2*J-1)])) * exp((1 - beta[J+1-i]) *
    (v2[J] - w2[J-i+1])/2 + s2.post[i-1]/2)
  }
  PIC.UltTot <- sum(PIC.Ult)

  # claims reserves
  PIC.Ris <- PIC.Ult - diagP
  PIC.RisTot <- sum(PIC.Ris)

  # prediction uncertainty
  msep <- 0
  for (i in 2:J) {
    for (k in 2:J) {
      if (i==k) {
        msep <- msep + (exp((1-beta[J+1-i]) * (v2[J] - w2[J+1-i]) +
        E[i-1, ]%*% Ainv %*% E[k-1, ]) - 1) * PIC.Ult[i-1] * PIC.Ult[k-1]
      } else {
        msep <- msep + (exp(E[i-1, ] %*% Ainv %*% E[k-1, ]) - 1) *
        PIC.Ult[i-1] * PIC.Ult[k-1]	
      }
    }
  }
  PIC.se <- sqrt(msep)

  output <- list()
  
  output[["Ult.Loss.Origin"]] <- as.matrix(PIC.Ult)
  output[["Ult.Loss"]] <- as.numeric(PIC.UltTot)
  output[["Res.Origin"]] <- as.matrix(PIC.Ris)
  output[["Res.Tot"]] <- as.numeric(PIC.RisTot)
  output[["s.e."]] <- as.numeric(PIC.se)
  
  return(output)
}


# Bayesian model code from Mario Wuthrich
# 
# #############################################################      
# ####### functions for parameter initialization
# #############################################################      
# 
# param.empirical <- function(xi, I0, J0) {
#   param <- array(0, c(J0, 2))
#   for (j in 1:J0){
#     param[j,1] <- mean(xi[(1:(I0-j+1)),j])       
#     if (j<I0) {param[j,2] <- sd(xi[(1:(I0-j+1)),j])
#     } else { 
#       param[j,2]= min(param[(j-1),2], param[(j-2),2], param[(j-1),2]^2/param[(j-2),2])
#     }}                  
#   param
# }           
# 
# 
# Sigma.matrix <- function(sigma, I0, J0){
#   Sigma <- array(0, c(I0*J0, I0*J0))
#   for (i1 in 1:I0){
#     for (j1 in 1:J0){
#       Sigma[(i1-1)*J0+j1,(i1-1)*J0+j1] <- sigma[j1]^2
#     }}
#   Sigma
# }
# 
# 
# T_matrix <- function(vco, theta, I0, J0){
#   T1 <- array(0, c(I0+J0, I0+J0))  
#   for (i1 in 1:(I0+J0)){
#     T1[i1,i1] <- theta[i1]^2 * vco^2 
#   }
#   T1
# }
# 
# #############################################################      
# ####### matrices and projections
# #############################################################      
# 
# A_matrix_cross_classified <- function(I0, J0){
#   A <- array(0, c(I0*J0, I0+J0))
#   for (i in 1:I0){
#     for (j in 1:J0){
#       A[(i-1)*J0+j,i] <- 1
#       A[(i-1)*J0+j,I0+j] <- 1
#     }}
#   A
# }         
# 
# A_matrix_CL <- function(I0, J0){
#   A <- array(0, c(I0*J0, I0+J0))
#   for (i in 1:I0){
#     for (j in 1:J0){
#       if (j==1){A[(i-1)*J0+j,i] <- 1}
#       A[(i-1)*J0+j,I0+j] <- 1
#     }}
#   A
# }         
# 
# 
# P_1 <- function(I0, J0){ 
#   P_1 <- array(0, c(I0*J0, I0*J0))
#   n1 <- 0
#   for (i in 1:I0) {
#     for (j in 1:(min(I0-i+1, J0))){
#       n1 <- n1 + 1
#       P_1[n1, (i-1)*J0 + j] <- 1
#     }}
#   P_1[1:n1,]
# }        
# 
# 
# P_2 <- function(I0, J0){
#   P_2 <- array(0, c(I0*J0, I0*J0))
#   n2 <- 0
#   for (i in 1:I0) {
#     if ((I0-i+1)< J0){
#       for (j in (I0-i+2): J0){
#         n2 <- n2 + 1
#         P_2[n2, (i-1)*J0 + j] <- 1
#       }}}
#   P_2[1:n2,]
# }
# 
# 
# #############################################################      
# ####### load data cross-classified log-normal case
# #############################################################      
# 
# data.x <- read.table("data_increments.csv", header=FALSE, sep=";")
# I0 <- nrow(data.x)
# J0 <- ncol(data.x)
# data.prior <- read.table("data_prior.csv", header=FALSE, sep=";")
# 
# xi <- array(0, c(I0*J0,1))
# theta <- array(0, c(I0+J0,1))
# 
# for (i in 1:I0) {
#   theta[i,1] <- log(data.prior[i, 1]) 
#   for (j in 1:(min(I0-i+1, J0))){
#     xi[(i-1)*J0+j,1] <- log(data.x[i,j])
#   }}      
# 
# #############################################################      
# ####### calculate parameters
# #############################################################      
# 
# param <- array(0, c(J0, 2))
# param <- param.empirical(log(data.x), I0, J0)
# sigma <- array(0, c(J0,1))
# sigma <- param[,2]
# normalization <- array(0, dim=c(J0, 1)) 
# for (j in 1:J0){normalization[j,1]<- exp(param[j,1]+param[j,2]^2/2)}
# theta[(I0+1):(I0+J0),1] <- param[,1] - log(sum(normalization[,1]))
# 
# A <- array(0, c(I0*J0, I0+J0))
# A <- A_matrix_cross_classified(I0, J0)
# mu <- A %*% theta
# 
# Sigma <- array(0, c(I0*J0, I0*J0))
# Sigma <- Sigma.matrix(sigma, I0, J0)
# T1 <- array(0, c(I0+J0, I0+J0)) 
# vco <- 1 
# T1 <- T_matrix(vco, theta, I0, J0)
# S <- array(0, c(I0*J0, I0*J0))
# S <- Sigma + A %*% T1 %*% t(A)
# P1 <- P_1(I0,J0)
# P2 <- P_2(I0,J0)  
# S11 <- P1 %*% S %*% t(P1)
# S11_inv <- solve(S11)
# S22 <- P2 %*% S %*% t(P2)
# S12 <- P1 %*% S %*% t(P2)
# N2 <- nrow(P2)
# 
# #############################################################      
# ####### calculate posterior parameters
# #############################################################      
# 
# mu2_post <- array(0, c(N2,1))
# S22_post <- array(0, c(N2,N2))
# mu2_post <- P2 %*% mu + t(S12) %*% S11_inv %*% (P1 %*% xi - P1 %*% mu)
# S22_post <- S22 - t(S12) %*% S11_inv %*% S12
# 
# #############################################################      
# ####### calculate reserves
# #############################################################      
# 
# result <- array(0, c(2,1))
# for (j1 in (1:N2)){
#   result[1,1] <- result[1,1] + exp(mu2_post[j1,1]+S22_post[j1,j1]/2)
#   for (j2 in (1:N2)){
#     result[2,1] <- result[2,1] + exp(mu2_post[j1,1]+S22_post[j1,j1]/2)*exp(mu2_post[j2,1]+S22_post[j2,j2]/2)*(exp(S22_post[j1,j2])-1)
#   }}
# result[2,1] <- sqrt(result[2,1])
# 
# round(result,0)
# 
# 
# 
# 
# 
# #############################################################      
# ####### load data multiplicative CL case
# #############################################################      
# 
# data.x <- read.table("data_cumulative.csv", header=FALSE, sep=";")
# I0 <- nrow(data.x)
# J0 <- ncol(data.x)
# data.prior <- read.table("data_prior.csv", header=FALSE, sep=";")
# 
# xi <- array(0, c(I0*J0,1))
# xi_ij <- array(0, c(I0,J0))
# C_ij  <- array(0, c(I0,J0))
# theta <- array(0, c(I0+J0,1))
# 
# for (i in 1:I0) {
#   theta[i,1] <- log(data.prior[i, 1]) 
#   for (j in 1:(min(I0-i+1, J0))){
#     if (j==1){xi[(i-1)*J0+j,1] <- log(data.x[i,j])
#     } else {
#       xi[(i-1)*J0+j,1] <- log(data.x[i,j]/data.x[i,j-1])
#     }
#     C_ij[i,j] <- data.x[i,j]            
#     xi_ij[i,j] <- xi[(i-1)*J0+j,1]            
#   }}
# 
# #############################################################      
# ####### calculate parameters
# #############################################################      
# 
# param <- array(0, c(J0, 2))
# param <- param.empirical(xi_ij, I0, J0)
# sigma <- array(0, c(J0,1))
# sigma <- param[,2]
# theta[(I0+1):(I0+J0),1] <- param[,1] - param[,2]^2/2
# theta[I0+1,1] <- theta[I0+1,1] - mean(theta[1:I0,1])
# 
# 
# A <- array(0, c(I0*J0, I0+J0))
# A <- A_matrix_CL(I0, J0)
# mu <- A %*% theta
# 
# Sigma <- array(0, c(I0*J0, I0*J0))
# Sigma <- Sigma.matrix(sigma, I0, J0)
# T1 <- array(0, c(I0+J0, I0+J0)) 
# vco <- 1 
# T1 <- T_matrix(vco, theta, I0, J0)
# S <- array(0, c(I0*J0, I0*J0))
# S <- Sigma + A %*% T1 %*% t(A)
# P1 <- P_1(I0,J0)
# P2 <- P_2(I0,J0)  
# S11 <- P1 %*% S %*% t(P1)
# S11_inv <- solve(S11)
# S22 <- P2 %*% S %*% t(P2)
# S12 <- P1 %*% S %*% t(P2)
# N2 <- nrow(P2)
# 
# #############################################################      
# ####### calculate posterior parameters
# #############################################################      
# 
# mu2_post <- array(0, c(N2,1))
# S22_post <- array(0, c(N2,N2))
# mu2_post <- P2 %*% mu + t(S12) %*% S11_inv %*% (P1 %*% xi - P1 %*% mu)
# S22_post <- S22 - t(S12) %*% S11_inv %*% S12
# 
# #############################################################      
# ####### calculate reserves
# #############################################################      
# 
# e_i <- array(0, c(I0, I0*J0))
# e_it <- array(0, c(I0, N2))
# for (i in (1:I0)){
#   for (j in (1:J0)){
#     e_i[i,(i-1)*J0+j ] <- 1
#   }
#   e_it[i,] <- P2 %*% e_i[i,]
# }
# 
# results <- array(0, c(2,1))
# for (i1 in ((I0-J0+1):I0)){
#   results[1,1] <- results[1,1] + C_ij[i1,I0-i1+1]*(exp( t(e_it[i1, ])%*% mu2_post[,1]+(t(e_it[i1, ])%*% S22_post %*% e_it[i1, ])/2)-1)
#   for (i2 in (I0-J0+1):I0){
#     x <- C_ij[i1,I0-i1+1]*exp( t(e_it[i1, ])%*% mu2_post[,1]+(t(e_it[i1, ])%*% S22_post %*% e_it[i1, ])/2)
#     x <- x * C_ij[i2,I0-i2+1]*exp( t(e_it[i2, ])%*% mu2_post[,1]+(t(e_it[i2, ])%*% S22_post %*% e_it[i2, ])/2)
#     x <- x * (exp( t(e_it[i1, ])%*% S22_post %*% e_it[i2, ])-1)
#     results[2,1] <- results[2,1] + x
#   }}
# results[2,1] <- sqrt(results[2,1]) 
# 
# 
# round(results,0)
# 
# 
# result[2,1]/result[1,1]
# results[2,1]/results[1,1]
