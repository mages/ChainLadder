test.print.BootChainLadder <- function() {
  # Test that print(x) returns x; see ?print
  res <- BootChainLadder(RAA)
  checkEquals(res, print(res))
}
