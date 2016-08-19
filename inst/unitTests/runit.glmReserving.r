test.glmReserve.DateLabels <- function() {
  expos <- (7 + 1:10 * 0.4) * 100
  GenIns2 <- GenIns
  rownames(GenIns2) <- paste0(2001:2010, "-01-01")
  attr(GenIns2, "exposure") <- expos
  (fit4 <- glmReserve(GenIns2))
  }

