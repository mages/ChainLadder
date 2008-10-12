\name{ChainLadder-package}
\alias{ChainLadder-package}
\alias{ChainLadder}
\docType{package}
\title{
  Various chain-ladder methods for claims reserving
}
\description{The ChainLadder-package grew out of presentations
  the author gave at the Stochastic Reserving Seminar at the Institute of Actuaries in November 2007.
  This package has currently implementations for the Mack-, Munich- and Bootstrap-chain-ladder methods.
  
  A presentation given at the R-user conference 2008 in Dortmund is available in the 
  \code{doc} folder of the installation directory of the ChainLadder package.

  The R command \code{searchpaths()[grep('ChainLadder', searchpaths())]} will tell you the exact path to the folder.
  
  An example spreadsheet is provided in the Excel folder of the above path and demonstrates how to use the \code{ChainLadder} 
  functions in Excel. For the the spreadsheet you will need the RExcel-Addin, see \url{http://sunsite.univie.ac.at/rcom/}.
  To install the Add-in get the R-package \code{RExcelInstaller} and call \code{installRExcel()}.
  
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

 RAA  

 MCL=MackChainLadder(RAA)
 MCL
 plot(MCL)

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


}

