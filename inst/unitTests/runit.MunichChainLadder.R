test.print.MunichChainLadder <- function() {
  # Test that print(x) returns x; see ?print
  res <- MunichChainLadder(MCLpaid, MCLincurred, est.sigmaP = 0.1, est.sigmaI = 0.1)
  checkEquals(res, print(res))
}

test.MCLweights <- function() {
  ## by Marco Spina
  ## 2022-06-27 Test the MunichChainLadder weights
  ## Create a matrix of weights for Last 3 LDF selection 
  t <- (row(MCLpaid) + col(MCLpaid) - 1)
  LDFselect <- ifelse(t <= max(col(MCLpaid)-4), 0, ifelse(t > max(col(MCLpaid)), NA, 1))
  MCL <- MunichChainLadder(Paid=MCLpaid,Incurred=MCLincurred, tailP = 1.05, tailI = 1, est.sigmaP="Mack", est.sigmaI="Mack", weights = LDFselect)
  Ult <- c(34055.98, 33291.31)
  
  ## test output from MunichChainLadder
  checkEquals(as.numeric(summary(MCL)$Totals[2,1:2]), Ult ,tol=1)
}
