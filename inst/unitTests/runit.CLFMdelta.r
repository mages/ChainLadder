test.CLFMdelta <- function() {
  # The example from the CLFM paper. Solution slightly different due to more relaxed tolerance.
  selected <- c(8.206,1.624, 1.275, 1.175, 1.115, 1.042, 1.035, 1.018, 1.009)
  soln <- c(2.000000, 1.000000, 1.158150, 1.305441, 1.116562, 1.000000, 3.000000, 2.000000, 1.000000)
  names(soln) <- 1:9
  attr(soln, "foundSolution") <- rep(TRUE, 9)
  all.equal(CLFMdelta(RAA, selected, tolerance = .0005), soln, tolerance = .0005, check.attributes = TRUE)
  }

