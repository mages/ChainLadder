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


## More for a laugh - 3d plot of a triangle and MackChainLadder output
if(require(rgl)){ #provides interactive 3d plotting functions
    MCL=MackChainLadder(GenIns/1e6, est.sigma="Mack")
    FT <- MCL$FullTriangle
    FTpSE <- FT+MCL$Mack.S.E
    FTpSE[which(MCL$Mack.S.E==0, arr.ind=TRUE)] <- NA
    FTmSE <- FT-MCL$Mack.S.E
    FTmSE[which(MCL$Mack.S.E==0, arr.ind=TRUE)] <- NA
    zr <- round(FT/FT[1,10]*100)
    zlim <- range(zr, na.rm=TRUE)
    zlen <- zlim[2] - zlim[1] + 1
    colorlut <- terrain.colors(zlen) # height color lookup table
    cols <- colorlut[ zr -zlim[1]+1 ] # assign colors to heights for each point
    x <- as.numeric(dimnames(FT)$origin)
    y <- as.numeric(dimnames(FT)$dev)
    persp3d(x, y=y, z=(FT), col=cols, xlab="origin", ylab="dev", zlab="loss",back="lines")
    mSE <- data.frame(as.table(FTmSE))
    points3d(xyz.coords(x=as.numeric(as.character(mSE$origin)), y=as.numeric(as.character(mSE$dev)),z=mSE$Freq), size=2)
    pSE <- data.frame(as.table(FTpSE))
    points3d(xyz.coords(x=as.numeric(as.character(pSE$origin)), y=as.numeric(as.character(pSE$dev)),z=pSE$Freq), size=2)
}
