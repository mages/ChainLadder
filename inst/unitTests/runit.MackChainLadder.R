test.getLatestCumulative.RAA <- function() {
    ## by Markus Gesmann
    ## Test the MackChainLadder against example given in Mack's 1999 paper
    MRT <- ChainLadder:::MackChainLadder(Mortgage, tail=1.05, tail.sigma=71, tail.se=0.02, est.sigma="Mack")
    ## Table 1 in the above paper
    f <- c(11.10, 4.092, 1.708, 1.276, 1.139, 1.069, 1.026, 1.023, 1.05)
    f.se <- c(2.24, 0.517, 0.122, 0.051, 0.042, 0.023, 0.015, 0.012, 0.02)
    F.se3 <- c(7.38, 1.89, 0.357, 0.116, 0.078, 0.033, 0.015, 0.007, 0.03)
    sig <- c(1337, 988.5, 440.1, 207, 164.2, 74.60, 35.49, 16.89,71)
    ## test output from MackChainLadder
    checkEquals(MRT$f, f,tol=0.001)
    checkEquals(MRT$f.se, f.se,tol=0.01)
    checkEquals(as.numeric(MRT$F.se[3,]),F.se3,tol=0.001)
    checkEquals(as.numeric(MRT$sigma),sig,tol=0.0001)
}
