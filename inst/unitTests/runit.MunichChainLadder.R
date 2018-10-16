test.print.MunichChainLadder <- function() {
  # Test that print(x) returns x; see ?print
  res <- MunichChainLadder(MCLpaid, MCLincurred, est.sigmaP = 0.1, est.sigmaI = 0.1)
  checkEquals(res, print(res))
}
