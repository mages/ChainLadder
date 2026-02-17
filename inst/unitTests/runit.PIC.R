test.PaidIncurredChain <- function() {
  ## Regression test for PaidIncurredChain using USAA data
  res <- PaidIncurredChain(USAApaid, USAAincurred)

  ## Golden values captured from PaidIncurredChain(USAApaid, USAAincurred)
  checkEquals(as.numeric(res$Ult.Loss.Origin),
    c(983367.242, 1078418.254, 1142422.790, 1241974.356, 1367661.648,
      1419425.001, 1406169.445, 1394320.234, 1324249.614),
    tol = 1)

  checkEquals(res$Ult.Loss, 11358008.58, tol = 1)
  checkEquals(res$Res.Tot, 1596953.58, tol = 1)
  checkEquals(res$"s.e.", 110976.85, tol = 1)

  ## Verify total ultimate equals sum of origin ultimates
  checkEquals(res$Ult.Loss, sum(res$Ult.Loss.Origin), tol = 1e-6)

  ## Verify total reserve equals sum of origin reserves
  checkEquals(res$Res.Tot, sum(res$Res.Origin), tol = 1e-6)
}
