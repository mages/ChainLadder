\name{QuantileIFRS17}
\alias{QuantileIFRS17}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile estimation for the IFRS 17 Risk Adjustment
}
\description{
The Quantile IFRS 17 function provides an estimate of the quantile attained on the reserve risk distribution that corresponds to the booked Risk Adjustment plus the Best Estimate.
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
  Correlation matrix depicting the correlations between each triangle imported. The correlation matrix is of dimension n x n, with n the number of items in the list of MackChainLadder objects. For correlation estimations between P&C risks, please refer to the article of Arbenz et al. below.
}
  \item{RiskMargin}{
  Input the risk margin as a single number. The risk margin corresponds to the IFRS 17 risk adjustment. It is estimated outside this function and can come from e.g. Solvency 2 standard formula. See International Actuarial Association reference below for details on risk adjustment calculations.
}
}
\details{
The IFRS 17 quantile is a mandatory disclosure when producing Financial Statements under the IFRS 17 framework: Such quantile reflects the Probability of Sufficiency of the reserves defined as Best Estimate plus Risk Adjustment i.e. the probability that the reserves will cover any negative deviations up to the disclosed quantile. 

When a risk measure other than the quantile measure (Value At risk) is used for determining the Risk Adjustment, the quantile has to be estimated. The purpose of this function is to provide such an estimation on deriving the first three moments of the reserve risk distribution. These moments are estimated on the triangles input into the function. These triangles are projected using chain-ladder methods and the standard Best Estimate, Mack volatility and skewness are estimated. The resulting moments of the different triangles are then aggregated using Fleishman polynomials.  

On using a Cornish-Fisher expansion based on the three aggregated moments, the Probability of Sufficiency of the reserves including the Risk Adjustment (given as an input to the function) can be easily derived.   
}
\value{
QuantileIFRS17 returns a vector with the following elements
\item{QuantileIFRS_17}{Quantile attained on the reserve risk distribution with the booked Risk Adjustment}
\item{Skewness}{Skewness of the overall aggregated risk distribution across all triangles}
\item{CoV}{Coefficient of Variation of the overall aggregated risk distribution across all triangles}
\item{Reserve}{Sum of reserves of the input MackChainLadder objects}
%\item{Totals}{A summary of the above elements: Quantile attained on the reserve risk distribution with the booked Risk Adjustment, Skewness, Coefficient of Variation and Total Reserves}
}
\references{
\cite{Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. \emph{Astin Bulletin}. Vol. 23. No 2. 1993. pp.213:225}

\cite{Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. \emph{Astin Bulletin}. Vol. 29. No 2. 1999. pp.361:366}

\cite{Dal Moro, Krvavych.
  Probability of sufficiency of Solvency II Reserve risk margins: Practical approximations. \emph{ASTIN Bulletin}, 47(3), 737-785
  }
  
\cite{P. Arbenz, D. Canestraro (2012) 
Estimating Copulas for Insurance from Scarce Observations, Expert Opinion and Prior Information: A Bayesian Approach, \emph{Astin Bulletin}, vol. 42(1), pages 271-290.
  }

\cite{International Actuarial Association (2018)
Risk Adjustments for Insurance Contracts under IFRS 17
   }

}
\author{
Eric Dal Moro, Yuriy Krvavych
}
\note{
The use of Fleishman polynomials and Cornish-Fisher expansion imply that the different risks involved in the triangles inputs should be "close to normality". If the risks involved in the input triangles are far from normal distributions (e.g. extreme events, nat cats ...), the proposed framework will not apply and the quantile derived from the function will not be relevant.   
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{MackChainLadder}}, 
  \code{\link{quantile.MackChainLadder}}
}
\examples{

QuantileIFRS17(MCL=list(M1=MackChainLadder(RAA, est.sigma = "Mack"), 
         M2=MackChainLadder(GenIns/1000, est.sigma = "Mack")), 
         Correlation=matrix(c(1,0.3, 0.3, 1), ncol=2), 
         RiskMargin = 20000)
}