\name{ChainLadder-package}
\alias{ChainLadder-package}
\alias{ChainLadder}
\docType{package}
\title{
  Mack- and Munich-chain-ladder models for claims reserving
}
\description{The ChainLadder-package grew out of presentations
  the author gave at the Stochastic Reserving Seminar at the Institute of Actuaries in November 2007.
  This package implements the Mack- and Munich-chain-ladder model using weighted
  linear regression, see \code{\link{lm}}.
  
  An example spreadsheet of how you might want to use these 
  functions in Excel is available in the installation directory of this package.
  Usually along the line  \emph{C:/Programmes/R/R-Version/library/ChainLadder/Excel}.
  You need the RExcel-Addin, from \url{http://sunsite.univie.ac.at/rcom/}, in order to use the spreadsheet.
}
\details{
\tabular{ll}{
Package: \tab ChainLadder\cr
Type: \tab Package\cr
Version: \tab 0.1.2\cr
Date: \tab 2007-12-05\cr
License: \tab GPL version 2 or later\cr
}
}
\author{
Markus Gesmann

Maintainer: Markus Gesmann <markus.gesmann@gmail.com>
}
\references{
Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. Astin Bulletin. Vol. 23. No 2. 1993. pp.213:225

Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. Astin Bulletin. Vol. 29. No 2. 1999. pp.361:366

Gerhard Quarg and Thomas Mack. Munich Chain Ladder. Blatter DGVFM 26, Munich, 2004. 
}
\keyword{ package }

\examples{

 data(RAA)
 RAA  

 MCL=MackChainLadder(RAA)
 MCL
 plot(MCL)


 # Munich Chain Ladder
 data(MCLpaid)
 MCLpaid
 data(MCLincurred)
 MCLincurred
 
 MCL = MunichChainLadder(MCLpaid, MCLincurred)
 MCL
 plot(MCL)

}

