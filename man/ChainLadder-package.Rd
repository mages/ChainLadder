\name{ChainLadder-package}
\alias{ChainLadder-package}
\alias{ChainLadder}
\encoding{UTF-8}
\docType{package}
\title{
  Methods and Models for Claims Reserving
}
\description{
  The ChainLadder-package grew out of presentations given at the Stochastic 
  Reserving Seminar at the Institute of Actuaries in 2007 and 2008 and 
  followed by talks at CAS meetings in 2008 and 2010.
   
  More information is available on the project web site
  \url{http://code.google.com/p/chainladder/} 
    
  Brian Fannin's package \code{MRMR} provides tools to analyze 
  non-life loss reserves. It uses a set of S3 and S4 objects to store data, 
  models and predictions.
  
  Another package you may want to look into is \code{lossDev}. It
  implements a Bayesian time series loss development model, see 
  \url{http://lossdev.r-forge.r-project.org/}

  If you are also interested in loss distributions modeling, risk theory 
  (including ruin theory), simulation of compound hierarchical models
  and credibility theory check out the \code{actuar} package by
  C. Dutang, V. Goulet and M. Pigeon. 

  For more financial packages see also CRAN Task View 'Emperical Finance'
  at \url{http://cran.r-project.org/web/views/Finance.html}.
}
\author{
  Markus Gesmann, Wayne Zhang, Daniel Murphy, Alessandro Carrato
  
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

\cite{Bardis, Majidi, Murphy. A Family of Chain-Ladder Factor Models for Selected Link Ratios. \emph{Variance}. Pending. Variance 6:2, 2012, pp. 143-160. \url{http://www.variancejournal.org/issues/06-02/143.pdf}}

\cite{Modelling the claims development result for solvency purposes.
Michael Merz, Mario V. Wüthrich. Casualty Actuarial Society E-Forum, Fall 2008.}

\cite{Claims Run-Off Uncertainty: The Full Picture. Michael Merz, 
Mario V. Wüthrich. Swiss Finance Institute Research Paper No. 14-69. \url{http://ssrn.com/abstract=2524352}. 2014}

\cite{Markus Gesmann. Claims Reserving and IBNR. 
  \emph{Computational Actuarial Science with R}. Chapman and Hall/CRC. 2014}
}

\examples{
\dontrun{
  demo(ChainLadder)
  }
}
\keyword{ package }