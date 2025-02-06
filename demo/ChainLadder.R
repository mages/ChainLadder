## Demos for the ChainLadder package
## See also the demos for:
## demo(MackChainLadder)
## demo(DatabaseExamples)
## demo(MSOffice)

## Example triangle
RAA
plot(RAA)
plot(RAA, lattice=TRUE)
MCL <- MackChainLadder(RAA, est.sigma="Mack")
MCL
plot(MCL)
# plot developments by origin period
plot(MCL, lattice=TRUE)
# one year claims development result
CDR(MCL)

# BootChainLadder
B <- BootChainLadder(RAA, R=999, process.distr="gamma")
B
plot(B)
# one year claims development result
CDR(B)

# fit a log-normal distribution
library(MASS)
# fit a log-normal distribution
fit <-  fitdistr(B$IBNR.Totals, "lognormal")
fit
plot(ecdf(B$IBNR.Totals))
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]), col="red", add=TRUE)

# Munich Chain Ladder
MCLpaid
MCLincurred

MCL <- MunichChainLadder(MCLpaid, MCLincurred, est.sigmaP="Mack", est.sigmaI="Mack")
MCL
plot(MCL)


# Working with triangles
# RAA is a matrix with additional class attribute 'triangle'
class(RAA)
# default plot for a triangle
plot(RAA)
# plot developments by origin period
plot(RAA, lattice=TRUE)

# Triangles can easily be converted into data.frames via
X=as.data.frame(RAA)
X
# Tables can also be converted into triangles
triangle <- as.triangle(X, origin="origin", dev="dev", value="value")
triangle

# One may also create triangles from data "by hand"
triangle("2012" = c(100, 150, 175, 180, 200),
         "2013" = c(110, 168, 192, 205),
         "2014" = c(115, 169, 202),
         "2015" = c(125, 185),
         "2016" = 150)
# Quick, simplified usage with a single vector of data
triangle(c(100, 150, 175, 180, 200,
           110, 168, 192, 205,
           115, 169, 202,
           125, 185,
           250))

