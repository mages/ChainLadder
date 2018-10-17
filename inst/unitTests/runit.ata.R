test.print.ata <- function() {
  # Test that print(x) returns x; see ?print
  res <- ata(GenIns)
  checkEquals(res, print(res))
}
