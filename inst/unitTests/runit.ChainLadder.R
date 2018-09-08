
test.getLatestCumulative.RAA <- function() {
    ## by Ben Escoto
    ## Test the getLatestCumulative function on RAA data
    ## First check one value from normal RAA data
    latest <- ChainLadder:::getLatestCumulative(RAA)
    checkEquals(latest[1], RAA[1, ncol(RAA)], checkNames=FALSE)

    ## Now try again after removing some early valuations
    RAA2 <- RAA
    RAA2[row(RAA) + col(RAA) <= 4] <- NA
    latest2 <- ChainLadder:::getLatestCumulative(RAA2)
    checkEquals(latest2[1], RAA2[1, ncol(RAA2)], checkNames=FALSE)
}

test.getLatestCumulative.simple <- function() {
    ## by Ben Escoto
    ## Test the getLatestCumulative function on simple toy data set
    ## 11/2/2012 remove attributes of new version of getLatestCumulative
    ##           before comparing -- Dan Murphy
    ## First, a traditional triangle
    simple.tri1 <- rbind(c(1,2), c(3, NA))
    latest <- ChainLadder:::getLatestCumulative(simple.tri1)
    checkEquals(c(latest), c(2,3), checkNames=FALSE)

    ## Now check very simple "trapezoidal" triangle
    simple.tri2 <- rbind(c(NA ,2), c(3, NA))
    latest2 <- ChainLadder:::getLatestCumulative(simple.tri2)
    checkEquals(c(latest2), c(2,3), checkNames=FALSE)
}

test.tailfactor <- function () {
    ## by Ben Escoto
    ## Test the tailfactor function for generating tail factors
    clratios <- c(1.2, 1.1, 1.05)
    tf <- ChainLadder:::tailfactor(clratios)$tail.factor
    checkEquals(tf, 1.050839, tolerance=1e-4)
}


test.asTriangles <- function () {
	## Following a bug report by Ben Escoto
	## In the old version as.triangle didn't work when development periods
	## were in different units, e.g. 10's and 100's and not ordered
	data(RAA)
	dimnames(RAA)$dev <- c(1,1e1,1e2,1e3,1e4,1e5,1e6,1e7,1e8,1e9)
	reverseRAA <- RAA[,10:1]
	zRAA <- as.triangle(as.data.frame(reverseRAA,na.rm=TRUE))
	any(dimnames(RAA)$dev==dimnames(zRAA)$dev)	
	
}

test.names_of_chainladder_Triangle <- function () {
  # Prior to April 2013 the column names of the triangle returned by
  # chainladder() were integers 1:n. Now they should be the same as 
  # the column names of the argument triangle.
	x <- chainladder(RAA)
  colnames(x) <- 12*(as.numeric(colnames(x)))
  y <- chainladder(x)
	checkEquals(colnames(y$Triangle), colnames(x))
}

test.limitOrigins<- function () {
  
  # test limiting origin on RAA triangle
  x <-sapply(chainladder(RAA, weights = limitOrigins(RAA,1,4))$Models, coefficients)
  checkEquals(x, c(3.48, 1.913, 1.266, 1.158, 1.1, 1.042, 1.033, 1.017, 1.0092), checkNames=FALSE, tolerance =0.001)
  
  # test limiting origin using a vector on RAA triangle
  y <-sapply(chainladder(RAA, weights = limitOrigins(RAA,1,c(3,4,3,2,1)))$Models, coefficients)
  checkEquals(y, c(sum(RAA[7:9,2])/sum(RAA[7:9,1]), sum(RAA[5:8,3])/sum(RAA[5:8,2]), sum(RAA[5:7,4])/sum(RAA[5:7,3]),
                   sum(RAA[5:6,5])/sum(RAA[5:6,4]), RAA[5,6]/RAA[5,5], RAA[4,7]/RAA[4,6], RAA[3,8]/RAA[3,7],
                   RAA[2,9]/RAA[2,8], RAA[1,10]/RAA[1,9]), checkNames=FALSE)
  
  
  # test on non std triangle
  tri_nonstd <- as.triangle(matrix(c(
    100, 200, 300, 400, 500,
    150, NA, 200, 400, NA,
    200, 250, NA, NA, NA,
    300, 350, NA, NA, NA,
    400, NA, NA, NA, NA
  ), nrow = 5, byrow = TRUE))
  z <-sapply(chainladder(tri_nonstd, weights = limitOrigins(tri_nonstd,1,1))$Models, coefficients)
  checkEquals(z, c(350/300, 1.5, 2,1.25), checkNames=FALSE)
}


