
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
    ## First, a traditional triangle
    simple.tri1 <- rbind(c(1,2), c(3, NA))
    latest <- ChainLadder:::getLatestCumulative(simple.tri1)
    checkEquals(latest, c(2,3), checkNames=FALSE)

    ## Now check very simple "trapezoidal" triangle
    simple.tri2 <- rbind(c(NA ,2), c(3, NA))
    latest2 <- ChainLadder:::getLatestCumulative(simple.tri2)
    checkEquals(latest2, c(2,3), checkNames=FALSE)
}

test.tailfactor <- function () {
    ## by Ben Escoto
    ## Test the tailfactor function for generating tail factors
    clratios <- c(1.2, 1.1, 1.05)
    tf <- ChainLadder:::tailfactor(clratios)$tail.factor
    checkEquals(tf, 1.050839, tolerance=1e-4)
}
