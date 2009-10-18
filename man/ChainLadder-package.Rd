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
  function to convert quickly from tables into triangles, triangles into
  tables, cumulative into incremental and incremental into cumulative triangles.
   
  The ChainLadder-package comes with an example spreadsheet which
  demonstrates how to use the \code{ChainLadder} 
  functions in Excel. The spreadsheet is located in the Excel folder of
  the package. The R command \code{searchpaths()[grep('ChainLadder',
    searchpaths())]} will tell you the exact path to the directory.
  For the the spreadsheet you will need the RExcel-Addin, see
  \url{http://sunsite.univie.ac.at/rcom/} for more details.
   
  More information and various presentations given about the ChainLadder-package are available
  on the project web site \url{http://code.google.com/p/chainladder/}
    
  If you are also interested in loss distributions modeling, risk theory (including ruin theory), 
  simulation of compound hierarchical models and credibility theory check out the \code{actuar} package 
  by C. Dutang, V. Goulet and M. Pigeon.
} 
\details{
\tabular{ll}{
Package: \tab ChainLadder\cr
Type: \tab Package\cr
Version: \tab 0.1.2\cr
Date: \tab 2008-10-12\cr
License: \tab GPL version 2 or later\cr
}
}
\author{
Markus Gesmann

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
MCL=MackChainLadder(RAA)
MCL
plot(MCL)
# plot developments by origin period
plot(MCL, lattice=TRUE)

# BootChainLadder
B <- BootChainLadder(RAA, R=999, process.distr="gamma")
B
plot(B)
# fitdistribution
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
}

