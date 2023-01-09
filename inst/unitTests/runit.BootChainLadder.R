test.print.BootChainLadder <- function() {
  # Test that print(x) returns x; see ?print
  res <- BootChainLadder(RAA)
  checkEquals(res, print(res))
}

test.seed.BootChainLadder <- function() {
  # Test that print(x) returns x; see ?print
  B1 <- BootChainLadder(RAA, R = 10, seed = 123) 
  B2 <- BootChainLadder(RAA, R = 100, seed = 123) 
  checkEquals(B1$IBNR.Triangles[,,1], B2$IBNR.Triangles[,,1])
  checkEquals(B1$IBNR.Triangles[,,1], B2$IBNR.Triangles[,,1])
}
