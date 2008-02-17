\name{MunichChainLadder}
\alias{MunichChainLadder}
\alias{print.MunichChainLadder}
\alias{summary.MunichChainLadder}
\alias{plot.MunichChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Munich-Chain-Ladder Model }
\description{
  Munich-Chain-Ladder model to forecast IBNR claims based on a cumulative
  paid and incurred claims triangle. 
  The Munich-Chain-Ladder model assumes that the Mack-model is applicable
  to the paid and incurred claims triangle, see \code{\link{MackChainLadder}}.
}
\usage{
MunichChainLadder(Paid, Incurred)

\method{print}{MunichChainLadder}(x, \dots)

\method{plot}{MunichChainLadder}(x,  mfrow=c(2,2), title=NULL, \dots)

\method{summary}{MunichChainLadder}(object, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Paid}{cumulative paid claims triangle. A quadratic (nxn)-matrix \eqn{P_{ik}} 
    which is filled for \eqn{k \leq n+1-i, i=1,\ldots,n }}
  \item{Incurred}{cumulative incurred claims triangle. A quadratic (nxn)-matrix \eqn{I_{ik}} 
    which is filled for \eqn{k \leq n+1-i, i=1,\ldots,n }}
  \item{x, object}{an object of class \code{"MunichChainLadder"}}
  \item{mfrow}{see \code{\link{par}}}
  \item{title}{see \code{\link{title}}}  
  \item{\dots}{not in use}

  
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  MunichChainLadder returns a list with the following values
  \item{Paid}{input paid triangle}
  \item{Incurred}{input incurred triangle}
  \item{MCLPaid}{Munich-chain-ladder forecasted full triangle on paid data}
  \item{MCLIncurred}{Munich-chain-ladder forecasted full triangle on incurred data}
  \item{SCLPaid}{standard-chain-ladder forecasted full triangle on paid data}
  \item{SCLIncurred}{standard-chain-ladder forecasted full triangle on
    Incurred data}
  \item{PaidResiduals}{paid residuals}
  \item{IncurredResiduals}{incurred residuals}
  \item{QResiduals}{paid/incurred residuals}
  \item{QinverseResiduals}{incurred/paid residuals}
  \item{lambdaP}{dependency coefficient between paid chain ladder ratios and
    incurred/paid ratios}
  \item{lambdaI}{dependency coefficient between incurred chain ladder ratios and
    paid/incurred ratios}
}
\references{
  Gerhard Quarg and Thomas Mack. Munich Chain Ladder. Blatter DGVFM 26,
  Munich, 2004. 
  }
\author{ Markus Gesmann \email{markus.gesmann@web.de} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See also \code{\link{MackChainLadder}} }
\examples{

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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
