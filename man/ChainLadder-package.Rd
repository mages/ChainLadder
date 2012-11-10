\name{ChainLadder-package}
\alias{ChainLadder-package}
\alias{ChainLadder}
\docType{package}
\title{
  Various chain-ladder methods for claims reserving
}
\description{
  The ChainLadder-package grew out of presentations
  given at the Stochastic Reserving Seminar at the Institute
  of Actuaries in 2007 and 2008 and followed by talks at CAS meetings in
  2008 and 2010.
  This package has currently implementations for the Mack-, Munich- and
  Bootstrap-chain-ladder methods. The package offers also some utility
  functions to convert quickly tables into triangles, triangles into
  tables, cumulative into incremental and incremental into cumulative
  triangles. 
  
  Since version 0.1.4-0 the package also includes the 
  "LDF Curve Fitting" methods of David Clark's paper 
  in the 2003 CAS \emph{Forum}. 
   
  The ChainLadder-package comes with an example spreadsheet which
  demonstrates how to use the \code{ChainLadder} 
  functions in Excel. The spreadsheet is located in the Excel folder of  
  the package. The R command \code{system.file("Excel",
  package="ChainLadder")} will tell you the exact path to the
  directory. 
  To use the spreadsheet you will need to have the RExcel-Addin, see 
  \url{http://sunsite.univie.ac.at/rcom/} for more details.  It also
  provides an example SWord file, demonstrating how the the functions of
  the package can be integrated into a MS Word file via SWord.  
  Again you find the Word file via the
  command:\code{system.file("SWord", package="ChainLadder")} 
   
  More information is available on the project web site
  \url{http://code.google.com/p/chainladder/} 
    
  If you are also interested in loss distributions modeling, risk theory 
  (including ruin theory), simulation of compound hierarchical models
  and credibility theory check out the \code{actuar} package by
  C. Dutang, V. Goulet and M. Pigeon. 

  Another package you might want to look into is \code{lossDev}. It
  implements a Bayesian time series loss development model. 
  Features include skewed-t distribution with time-varying scale
  parameter, reversible jump MCMC for determining 
  the functional form of the consumption path, and a structural break in
  this path; by Christopher W. Laws and Frank A. Schmid 
  see also \url{http://lossdev.r-forge.r-project.org/}
  
  For more financial packages see also CRAN Task View 'Emperical Finance'
  at \url{http://cran.r-project.org/web/views/Finance.html}.
  
} 
\details{
  \tabular{ll}{
    Package: \tab ChainLadder\cr
    Type: \tab Package\cr
    Version: \tab 0.1.5-4\cr
    Date: \tab 2012-11-10\cr
    License: \tab GPL version 2 or later\cr
  }
}
\author{
  Markus Gesmann, Wayne Zhang, Daniel Murphy
  
  Maintainer: Markus Gesmann <markus.gesmann@gmail.com>
}
\references{
  \cite{Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. \emph{Astin Bulletin.} Vol. 23. No 2. 1993. pp.213:225}
  
  \cite{Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. \emph{Astin Bulletin.} Vol. 29. No 2. 1999. pp.361:366}
  
  \cite{Gerhard Quarg and Thomas Mack. Munich Chain Ladder. \emph{Blatter DGVFM 26.} Munich. 2004.}
  
  \cite{England, PD and Verrall, RJ. Stochastic Claims Reserving in General Insurance (with discussion). \emph{British Actuarial Journal 8.} III. 2002 }
  
  \cite{B. Zehnwirth and G. Barnett. \emph{Best Estimates for Reserves}. \emph{Proceedings of the CAS.} Volume LXXXVII. Number 167.November 2000.}
  
  \cite{Clark, David R., "LDF Curve-Fitting and Stochastic Reserving: A Maximum Likelihood Approach," \emph{CAS Forum}, Fall 2003.}
  
\cite{Zhang Y. A general multivariate chain ladder model.\emph{Insurance: Mathematics and Economics}, 46, pp. 588:599, 2010.}

\cite{ Zhang, Y. Likelihood-based and Bayesian Methods for Tweedie Compound Poisson Linear Mixed Models, \emph{Statistics and Computing}, forthcoming. 
\url{http://www.actuaryzhang.com/publication/MixedTweedie.pdf}}

}

\examples{
\dontrun{
  demo(ChainLadder)
  }
}
\keyword{ package }


