\name{ChainLadder-package}
\alias{ChainLadder-package}
\alias{ChainLadder}
\docType{package}
\title{
  Various chain-ladder methods for claims reserving
}
\description{The ChainLadder-package grew out of presentations
  the author gave at the Stochastic Reserving Seminar at the Institute
  of Actuaries in November 2007 and 2008.
  This package has currently implementations for the Mack-, Munich- and
  Bootstrap-chain-ladder methods. The package offers also some utility
  functions to convert quickly tables into triangles, triangles into
  tables, cumulative into incremental and incremental into cumulative triangles.
   
  The ChainLadder-package comes with an example spreadsheet which
  demonstrates how to use the \code{ChainLadder} 
  functions in Excel. The spreadsheet is located in the Excel folder of
  the package. The R command \code{system.file("Excel", package="ChainLadder")} will tell you the exact path to the directory.
  To use the spreadsheet you will need to have the RExcel-Addin, see
  \url{http://sunsite.univie.ac.at/rcom/} for more details.
   
  More information is available
  on the project web site \url{http://code.google.com/p/chainladder/}
    
  If you are also interested in loss distributions modeling, risk theory (including ruin theory), 
  simulation of compound hierarchical models and credibility theory check out the \code{actuar} package 
  by C. Dutang, V. Goulet and M. Pigeon.

  lossDev - A Bayesian time series loss development model. Features include skewed-t distribution with time-varying scale parameter, Reversible Jump MCMC for determining the functional form of the consumption path, and a structural break in this path; by Christopher W. Laws and Frank A. Schmid
\url{http://lossdev.r-forge.r-project.org/}
  
  For more financial packages see also CRAN Task View 'Emperical Finance'
  at \url{http://cran.r-project.org/web/views/Finance.html}.
  
} 
\details{
\tabular{ll}{
Package: \tab ChainLadder\cr
Type: \tab Package\cr
Version: \tab 0.1.3-2\cr
Date: \tab 20010-02-17\cr
License: \tab GPL version 2 or later\cr
}
}
\author{
Markus Gesmann, Wayne (Yanwei) Zhang

Maintainer: Markus Gesmann <markus.gesmann@gmail.com>
}
\references{
\cite{Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. \emph{Astin Bulletin.} Vol. 23. No 2. 1993. pp.213:225}

\cite{Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. \emph{Astin Bulletin.} Vol. 29. No 2. 1999. pp.361:366}

\cite{Gerhard Quarg and Thomas Mack. Munich Chain Ladder. \emph{Blatter DGVFM 26.} Munich. 2004.}

\cite{England, PD and Verrall, RJ. Stochastic Claims Reserving in General Insurance (with discussion). \emph{British Actuarial Journal 8.} III. 2002 }

\cite{B. Zehnwirth and G. Barnett. \emph{Best Estimates for Reserves}. \emph{Proceedings of the CAS.} Volume LXXXVII. Number 167.November 2000.}
}
\keyword{ package }

\examples{

# Example triangle
RAA
plot(RAA)
plot(RAA, lattice=TRUE)
MCL=MackChainLadder(RAA, est.sigma="Mack")
MCL
plot(MCL)
# plot developments by origin period
plot(MCL, lattice=TRUE)

# BootChainLadder
B <- BootChainLadder(RAA, R=999, process.distr="gamma")
B
plot(B)
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
 
MCL = MunichChainLadder(MCLpaid, MCLincurred)
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
persp3d(x, y=y, 
        z=(FT), col=cols, xlab="origin", ylab="dev", zlab="loss",back="lines") 
mSE <- data.frame(as.table(FTmSE)) 
points3d(xyz.coords(x=as.numeric(as.character(mSE$origin)), 
    y=as.numeric(as.character(mSE$dev)),z=mSE$Freq), size=2) 
pSE <- data.frame(as.table(FTpSE)) 
points3d(xyz.coords(x=as.numeric(as.character(pSE$origin)), 
    y=as.numeric(as.character(pSE$dev)),z=pSE$Freq), size=2) 
}
}


