test.glmReserve.DateLabels <- function() {
  expos <- (7 + 1:10 * 0.4) * 10
  GenIns2 <- GenIns
  rownames(GenIns2) <- paste0(2001:2010, "-01-01")
  attr(GenIns2, "exposure") <- expos
  #checkException(fit5 <- glmReserve(GenIns2))
  names(expos) <- rownames(GenIns2)
  attr(GenIns2, "exposure") <- expos
  (fit6 <- glmReserve(GenIns2))
  }

test.print.glmReserve <- function() {
  # Test that print(x) returns x; see ?print
  res <- glmReserve(GenIns / 1000)
  checkEquals(res, print(res))
}