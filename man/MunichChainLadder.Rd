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

data(MCLpaid)
MCLpaid
data(MCLincurred)
MCLincurred

MCL = MunichChainLadder(MCLpaid, MCLincurred)
MCL
plot(MCL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
