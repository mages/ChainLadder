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

Maintainer: Markus Gesmann <markus.gesmann@web.de>
}
\references{
Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. Astin Bulletin. Vol. 23. No 2. 1993. pp.213:225

Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. Astin Bulletin. Vol. 29. No 2. 1999. pp.361:366

Gerhard Quarg and Thomas Mack. Munich Chain Ladder. Blatter DGVFM 26, Munich, 2004. 
}
\keyword{ package }

\examples{

# Example data sets

# Run off triangle of accumulated claims data. A data frame with 10 underwriting years and 10 
# development years.
# Source:Historical Loss Development, Reinsurance Association of America (RAA), 1991, p.96
# See Also: Which Stochastic Model is Underlying the Chain Ladder Method?, Thomas Mack, 1994,
# Insurance Mathematics and Economics, 15, 2/3, 133-138
#
# P.D.England and R.J.Verrall, Stochastic Claims Reserving in General Insurance, 
# British Actuarial Journal, Vol. 8, pp443-544, 2002
RAA <- t(matrix(c(
  5012,  8269, 10907, 11805, 13539, 16181, 18009, 18608, 18662, 18834,
   106,  4285,  5396, 10666, 13782, 15599, 15496, 16169, 16704,    NA,
  3410,  8992, 13873, 16141, 18735, 22214, 22863, 23466,    NA,    NA,
  5655, 11555, 15766, 21266, 23425, 26083, 27067,    NA,    NA,    NA,
  1092,  9565, 15836, 22169, 25955, 26180,    NA,    NA,    NA,    NA,
  1513,  6445, 11702, 12935, 15852,    NA,    NA,    NA,    NA,    NA,
   557,  4020, 10946, 12314,    NA,    NA,    NA,    NA,    NA,    NA,
  1351,  6947, 13112,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
  3133,  5395,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
  2063,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA
  ), ncol=10))
  


 MCL=MackChainLadder(RAA)
 MCL
 plot(MCL)


 # Munich Chain Ladder
Paid <- t(matrix(c(
  576, 1804, 1970, 2024, 2074, 2102, 2131,
  866, 1948, 2162, 2232, 2284, 2348,   NA,
 1412, 3758, 4252, 4416, 4494,   NA,   NA,
 2286, 5292, 5724, 5850,   NA,   NA,   NA,
 1868, 3778, 4648,   NA,   NA,   NA,   NA,
 1442, 4010,   NA,   NA,   NA,   NA,   NA,
 2044,   NA,   NA,   NA,   NA,   NA,   NA
 ), ncol=7))
 	
Incurred <- t(matrix(c( 
  978, 2104, 2134, 2144, 2174, 2182, 2174,
 1844, 2552, 2466, 2480, 2508, 2454,   NA,
 2904, 4354, 4698, 4600, 4644,   NA,   NA,
 3502, 5958, 6070, 6142,   NA,   NA,   NA,
 2812, 4882, 4852,   NA,   NA,   NA,   NA,
 2642, 4406,   NA,   NA,   NA,   NA,   NA,
 5022,   NA,   NA,   NA,   NA,   NA,   NA
 ), ncol=7))

MCL = MunichChainLadder(Paid, Incurred)
MCL
plot(MCL)




}

