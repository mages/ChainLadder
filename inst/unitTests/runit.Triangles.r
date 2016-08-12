test.Triangles.DateLabels <- function() {
  # by Brian and Dan
  op <- as.Date(paste0(2001:2010, "-01-01"))
  lags <- 1:10
  
  triangle <- expand.grid(op, lags)
  names(triangle) <- c("origin", "dev")
  set.seed(1234)
  triangle$value <- rnorm(100)
  
  triCL <- ChainLadder::as.triangle(triangle)
  plot(triCL, lattice=TRUE)
}
