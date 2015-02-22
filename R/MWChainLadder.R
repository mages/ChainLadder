CDR.MackChainLadder <- function(x,...){  
  Mack <- x
  if(!"MackChainLadder" %in% class(Mack))
    stop("The input to CDR.MackChainLadder has to be output of MackChainLadder.")
  if(!all(Mack$alpha==1))
    warning("The Merz & Wuthrich forumlae hold only for alpha=1.")
  
  Trian <- Mack$Triangle
  bool <- "MWpaper"
  ##  variables
  I <- nrow(Trian)
  J <- ncol(Trian)
  diag <- matrix(NA,I,1)
  diag_inv <- matrix(NA,I,1)
  S_I <- matrix(NA,1,J)
  S_II <- matrix(NA,1,J)
  Phi <- matrix(NA,I,1)
  Psi <- matrix(NA,I,1) 
  Delta <- matrix(NA,I,1)
  Gamma <- matrix(NA,I,1)
  Lambda <- matrix(NA,I,1)
  Upsilon <- matrix(NA,I,1)
  cov_obs <- matrix(0,I,J)
  cov_reel <- matrix(0,I,J)
  msep_obs <- matrix(NA,1,J+1)
  msep_reel <- matrix(NA,1,J+1)
  Delta[1] <- Phi[1] <- Psi[1] <- Upsilon[1] <- Lambda[1] <- 0
    
  Mack$sigma[J-1] <- sqrt(min(Mack$sigma[J-2]^4/
                                Mack$sigma[J-3]^2,min(Mack$sigma[J-2]^2,Mack$sigma[J-3]^2)))
  #plot(Mack)
  
  
  for (i in 1:I){
    diag[i] = Trian[i,I-i+1]
    diag_inv[i] = Trian[I-i+1,i]
  }
  
  for (j in 1:J){
    S_I[j] <- sum(Trian[1:(I-j),j])
    S_II[j] <- sum(Trian[1:(I-j+1),j])
  }
  S_I[I] <- 0
  
  Delta[2] <- Mack$sigma[I-1]^2/(S_I[I-1]*(Mack$f[I-1])^2)
  Phi[2] <- 0
  Psi[2] <- Mack$sigma[I-1]^2/(diag[2]*(Mack$f[I-1])^2)
  Upsilon[2] <- Mack$sigma[I-1]^2/(S_II[I-1]*(Mack$f[I-1])^2)
  Lambda[2] <- diag[2]*Mack$sigma[I-1]^2/((Mack$f[I-1]^2)*S_II[I-1]*S_I[I-1])
  for (i in 3:I){
    Delta[i] <- Mack$sigma[I-i+1]^2/(S_I[I-i+1]*(Mack$f[I-i+1])^2) + sum(
      (diag_inv[(I-i+2):(J-1)]/S_II[(I-i+2):(J-1)])^2*
        Mack$sigma[(I-i+2):(J-1)]^2/(S_I[(I-i+2):(J-1)]*(Mack$f[(I-i+2):(J-1)])^2))        
    Phi[i] <- sum((diag_inv[(I-i+2):(J-1)]/S_II[(I-i+2):(J-1)])^2*
                    Mack$sigma[(I-i+2):(J-1)]^2/(diag_inv[(I-i+2):(J-1)]*(Mack$f[(I-i+2):(J-1)])^2))    
    Psi[i] <- Mack$sigma[I-i+1]^2/(diag[i]*(Mack$f[I-i+1])^2)
    Upsilon[i] <- Phi[i] + Mack$sigma[I-i+1]^2/(S_II[I-i+1]*(Mack$f[I-i+1])^2)
    
    Lambda[i] <- diag[i]*Mack$sigma[I-i+1]^2/((Mack$f[I-i+1]^2)*S_II[I-i+1]*S_I[I-i+1]) + sum(
      (diag_inv[(I-i+2):(J-1)]/S_II[(I-i+2):(J-1)])^2*
        Mack$sigma[(I-i+2):(J-1)]^2/(S_I[(I-i+2):(J-1)]*(Mack$f[(I-i+2):(J-1)])^2))
  }
  Gamma = Phi + Psi
  
  #MSEP par acc. year
  for (i in 1:I){
    msep_obs[i] = Mack$FullTriangle[i,J]^2 * (Gamma[i] + Delta[i])
    msep_reel[i] = Mack$FullTriangle[i,J]^2 * (Phi[i] + Delta[i])
  }
  
  #Covariance
  for (i in 2:(I-1)){ 
    for (k in (i+1):I){
      cov_obs[i,k] <- Mack$FullTriangle[i,J]*Mack$FullTriangle[k,J]*(Upsilon[i] + Lambda[i])
      cov_reel[i,k] <- Mack$FullTriangle[i,J]*Mack$FullTriangle[k,J]*(Phi[i] + Lambda[i])    
    }
  }
  
  #MSEP aggregated  
  msep_obs[I+1] = sum(msep_obs[2:I]) + 2*sum(cov_obs)
  # Something is not quite right here
  # It should be msep_reel = msep_ops - vari
  msep_reel[I+1] = sum(msep_reel[2:I]) + 2*sum(cov_obs)
  
  
  
  ##################################################################################################################
  ## EXACTES
  ##################################################################################################################
  #Declaration des variables
  facteur <- matrix(1,I,1)
  Phi_exact <- matrix(NA,I,1)
  Psi_exact <- matrix(NA,I,1)
  Gamma_exact <- matrix(NA,I,1)
  Upsilon_exact <- matrix(NA,I,1)
  cov_obs_exact <- matrix(0,I,J)
  cov_reel_exact <- matrix(0,I,J)
  msep_obs_exact <- matrix(NA,1,J+1)
  msep_reel_exact <- matrix(NA,1,J+1)
  Phi_exact[1] <- Upsilon_exact[1]<- Gamma_exact[1] <- 0
  
  # i=2
  Phi_exact[2] <- Psi_exact[2] <- 0
  Gamma_exact[2] <- Mack$sigma[I-1]^2/(diag[I-1]*(Mack$f[I-1])^2)
  Upsilon_exact[2] <- Mack$sigma[I-1]^2/(S_II[I-1]*(Mack$f[I-1])^2)
  
  # loop for prior
  for (i in 3:I){
    facteur[i] <-prod(1 + diag_inv[(I-i+2):(J-1)]*(Mack$sigma[(I-i+2):(J-1)]^2)/((S_II[(I-i+2):(J-1)]*
                                                                                    Mack$f[(I-i+2):(J-1)])^2))
    Phi_exact[i] <- (1 + Mack$sigma[I-i+1]^2/(diag_inv[I-i+1]*(Mack$f[I-i+1])^2))* (facteur[i]-1)    
    Psi_exact[i] <- (1 + Mack$sigma[I-i+1]^2/(S_II[i]*(Mack$f[I-i+1])^2)) * Phi[i] /
      (1 + Mack$sigma[I-i+1]^2/(diag_inv[I-i+1]*(Mack$f[I-i+1])^2))
    Upsilon_exact[i] <- (1 + Mack$sigma[I-i+1]^2/(S_II[I-i+1]*(Mack$f[I-i+1])^2))* facteur[i]-1    
    Gamma_exact[i] <- (1 + Mack$sigma[I-i+1]^2/(diag_inv[I-i+1]*(Mack$f[I-i+1])^2))* facteur[i]-1    
  }
  
  #MSEP per accident year
  for (i in 1:I){
    msep_obs_exact[i] = Mack$FullTriangle[i,J]^2 * (Gamma_exact[i] + Delta[i])
    msep_reel_exact[i] = Mack$FullTriangle[i,J]^2 * (Phi_exact[i] + Delta[i])
  }
  
  #Covariance
  for (i in 2:(I-1)){ 
    for (k in (i+1):I){
      cov_obs_exact[i,k] <- Mack$FullTriangle[i,J]*Mack$FullTriangle[k,J]*(Upsilon_exact[i] + Lambda[i])
      cov_reel_exact[i,k] <- Mack$FullTriangle[i,J]*Mack$FullTriangle[k,J]*(Psi_exact[i] + Lambda[i])   
    }
  }
  
  #MSEP ag.  
  msep_obs_exact[I+1] <- sum(msep_obs_exact[2:I]) + 2*sum(cov_obs_exact)
  
  ## Something is not quite right here  
  msep_reel_exact[I+1] <- sum(msep_reel_exact[2:I]) + 2*sum(cov_obs_exact)

  
  
  ##################################################################################################################
  ##Outputs Function 
  ##################################################################################################################
  msep_Mack <- array(0,c(1,I+1))
  msep_Mack[1:I] <- Mack$Mack.S.E[,I]
  msep_Mack[I+1] <- Mack$Total.Mack.S.E
  
  Vari <- array(0,c(1,I+1))
  for (i in 1:I){
    Vari[i] <- Mack$FullTriangle[i,J]^2 * Psi[i]
  }
  Vari[I+1] <- sum(Vari[1:I])
  
  # complete vision
  if(bool==1){    
    result <- cbind(t(msep_Mack), t(sqrt(Vari)), t(sqrt(msep_obs)), t(sqrt(msep_obs_exact)), t(sqrt(msep_reel)), t(sqrt(msep_reel_exact)))
    result <- as.data.frame(result)
    l <- list("Variance CDR", "MSEP Mack","MSEP obs. approx.","MSEP obs. exact", "MSEP real approx.","MSEP real exact")
    names(result) <- l  
   }
  
  #Var <- rev(Mack$Mack.ProcessRisk[row(Trian) == (J + 1 - col(Trian))])
  
  #si l'on souhaite uniquement l'aspect solvabilite
  if(bool==0){
    result <- cbind(t(msep_Mack), t(sqrt(msep_obs)), t(sqrt(msep_obs_exact)))
    result <- as.data.frame(result)
    l <- list("MSEP Mack","MSEP CDR approx","MSEP CDR exact")
    names(result) <- l
  }
  
  if(bool=="MWpaper"){
  ## Follow output of table 4 on page 562 in the 2008 MW paper 
    reserve_Mack <- array(0,c(1,I+1))
    reserve_Mack[1:I] <- summary(Mack)$ByOrigin$IBNR
    reserve_Mack[I+1] <- summary(Mack)$Total[4,1]
    
    result <- cbind(t(reserve_Mack),
                    #t(sqrt(Vari)), # Process
                    #t(sqrt(msep_reel)), # Parameter 
                    t(sqrt(msep_obs)),                   
                    #t(sqrt(msep_obs_exact)), 
                    #t(sqrt(msep_reel_exact)),
                    t(msep_Mack))
    
    result <- as.data.frame(result)
     names(result) <- c("IBNR",
                        #"MSEP.CDR.Process",                         
                        #"MSEP.CDR.Parameter",                        
                        "CDR.S.E",
                        "Mack.S.E")    
   }
  
  rownames(result) <- c(rownames(Trian), "Total")
  return(result)
  
}