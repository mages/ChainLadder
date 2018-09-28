\name{QuantileIFRS17}
\alias{QuantileIFRS17}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile estimation for the IFRS 17 Risk Adjustment
}
\description{
The Quantile IFRS 17 function provides an estimate of the quantile attained on the reserve risk distribution that corresponds to the booked Risk Adjustment amount on top of the Best Estimate.
}
\usage{
QuantileIFRS17(MCL, Correlation, RiskMargin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MCL}{
  a list of \code{MackChainLadder} objects
}
  \item{Correlation}{
  Correlation matrix depicting the correlations between each triangle imported
}
  \item{RiskMargin}{
  Input the risk margin as a single number
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
QuantileIFRS17 returns a vector with the following elements
\item{QuantileIFRS_17}{Quantile attained on the reserve risk distribution with the booked Risk Adjustment}
\item{Skewness}{Skewness of the overall aggregated risk distribution across all triangles input}
\item{CoV}{Coefficient of Variation of the overall aggregated risk distribution across all triangles input}
\item{Res}{Best Estimate reserve using a chain-ladder model applied on all input triangles}
%\item{Totals}{A summary of the above elements: Quantile attained on the reserve risk distribution with the booked Risk Adjustment, Skewness, Coefficient of Variation and Total Reserves}
}
\references{
\cite{Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. \emph{Astin Bulletin}. Vol. 23. No 2. 1993. pp.213:225}

\cite{Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. \emph{Astin Bulletin}. Vol. 29. No 2. 1999. pp.361:366}

\cite{Dal Moro, Krvavych.
  Probability of sufficiency of Solvency II Reserve risk margins: Practical approximations. \emph{ASTIN Bulletin}, 47(3), 737-785
  }
}
\author{
Eric Dal Moro
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{MackChainLadder}}, 
  \code{\link{quantile.MackChainLadder}}
}
\examples{


QuantileIFRS17(MCL=list(M1=MackChainLadder(RAA, est.sigma = "Mack"), 
         M2=MackChainLadder(GenIns, est.sigma = "Mack")), 
         Correlation=matrix(c(1,0.3, 0.3, 1), ncol=2), 
         RiskMargin = 1000)
}