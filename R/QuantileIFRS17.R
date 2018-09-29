
## Authors: Eric Dal Moro / Yuriy Krvavych
## Copyright: Eric Dal Moro / Yuriy Krvavych
## Added 1 August 2018

QuantileIFRS17 <- function(MCL, Correlation, RiskMargin)
{
 # Check that all inputs MCL are MackChainLadder outputs 
  if(!all(sapply(MCL, function(x) {
    "MackChainLadder" %in% class(x)
  }))){
    stop("Please provide as input a list of MackChainLadder ojects.")
  }
  ## Check that we have at least two triangles
  if(length(MCL) < 2){
    stop("Please provide at least a list of at least two MackChainLadder objects.")
  }
  ## Check that the triangles are of the same dimensions
  dims <- sapply(MCL, function(x) dim(x$Triangle))
  
  if(!all((dims - max(dims))==0) ){
    stop("This function works only for triangles of the same dimensions with equal number of origin and development periods.")
  }
      
  # Check correlation matrix is of the correct dimension
  if(! (dim(Correlation)[1] == length(MCL) & dim(Correlation)[2] == length(MCL)))
    stop("The number of rows and columns of the correlation matrix should be the same as the number of MackChainLadder object provided.")
  
  # Dimensions of triangles
  ncol <- dim(MCL[[1]]$Triangle)[2]
  nlin <- dim(MCL[[1]]$Triangle)[1]
  # Number of triangles
  nbTriangle <- length(MCL) 
  
  Skew <- c(nbTriangle)
  Stdev <- c(nbTriangle)
  Reserve <- c(nbTriangle)
  Gamma <- c(nbTriangle)
  Phi <- c(nbTriangle)
  bi <- c(nbTriangle)
  ai <- c(nbTriangle)
  
  asm <- lapply(MCL, Asymetrie)
  Skew <- sapply(asm, "[[", "OverSkew")
  Stdev <- sapply(MCL, '[[', 'Total.Mack.S.E')
  Reserve <- sum(sapply(lapply(lapply(MCL, 'summary'), 
                           '[[', 'Totals'), t)[4,])
  
  Gamma <- Skew/Stdev^3
  Phi <- acos(-Gamma/sqrt(8))
  bi <- sqrt(2)*cos(Phi/3+4*pi/3)
  ai <- sqrt(1-2*bi^2)
  
  Variance <- sum(Stdev^2)
  Skewness <- sum(Skew)
  
  for (j in c(1:nbTriangle))  {
    for (k in c(1:nbTriangle))  {
      if (j != k) {
        Variance <- Variance+Stdev[j]*Stdev[k]*Correlation[j,k]*(ai[j]*ai[k]+2*bi[j]*bi[k]*Correlation[j,k])
        Skewness <- Skewness+3*Stdev[j]^2*Stdev[k]*2*Correlation[j,k]*(2*ai[j]*ai[k]*bi[j]+(ai[j]^2+4*bi[j]^2)*bi[k]*Correlation[j,k])
      }
    }
  }
  
  
  for (i in c(1:nbTriangle))  {
    for (j in c(1:nbTriangle))  {
      for (l in c(1:nbTriangle)) {
        if ((i != j) & (i !=l) & (j !=l)) {
          Skewness <- Skewness + Stdev[i]*Stdev[j]*Stdev[l]*(2*(ai[j]*ai[l]*bi[i]*Correlation[i,j]*Correlation[i,l]+ai[j]*ai[i]*bi[l]*Correlation[j,l]*Correlation[i,l]+ai[i]*ai[l]*bi[j]*Correlation[i,j]*Correlation[j,l])+8*bi[i]*bi[j]*bi[l]*Correlation[i,j]*Correlation[i,l]*Correlation[j,l])
        }
      }
    }  
  }
  
  GammaX <- Skewness/Variance^1.5
  q <- RiskMargin/Variance^0.5
  Za <- -3/GammaX+sqrt(9/GammaX^2+6*q/GammaX+1)
  
  output <-list(
    QuantileIFRS_17 = pnorm(Za),
    CoV = Variance^0.5/Reserve,
    Skewness = GammaX,
    Reserve = Reserve)
  
  output <- sapply(output, as.numeric)
  
  return(output)
}