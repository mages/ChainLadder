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
MunichChainLadder(Paid, Incurred, 
                  est.sigmaP = "loglinear", est.sigmaI = "loglinear", 
                  tailP=FALSE, tailI=FALSE)

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
  \item{est.sigmaP}{hello}
  \item{est.sigmaI}{hello}  
  \item{tailP}{hello}
  \item{tailI}{hello}
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
  \item{call}{the matched call} 
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
  \item{qinverse.f}{hello}
  \item{rhoP.sigma}{hello}
  \item{q.f}{hello}
  \item{rhoI.sigma}{hello}
}
\references{
  Gerhard Quarg and Thomas Mack. Munich Chain Ladder. Blatter DGVFM 26,
  Munich, 2004. 
  }
\author{ Markus Gesmann \email{markus.gesmann@gmail.com} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See also \code{\link{MackChainLadder}} }
\examples{

MCLpaid
MCLincurred

M = MunichChainLadder(MCLpaid, MCLincurred)
M
plot(M)
# You can access the standard chain ladder (Mack) output
M$SCLPaid
M$SCLIncurred

# Following the example output in Quarg's paper:
MCL <- MunichChainLadder(MCLpaid, MCLincurred, est.sigmaP=0.1, est.sigmaI=0.1)
# Input triangles
MCL$Paid
MCL$Incurred
#Standard chain ladder factors
MCL$SCLPaid$f
MCL$SCLIncurred$f
MCL$SCLPaid$sigma
MCL$SCLIncurred$sigma
# Check Mack's assumptions graphically
plot(MCL$SCLPaid)
plot(MCL$SCLIncurred)

MCL$q.f
MCL$rhoP.sigma
MCL$rhoI.sigma

MCL$PaidResiduals
MCL$IncurredResiduals

MCL$QinverseResiduals
MCL$QResiduals

MCL$lambdaP
MCL$lambdaI

MCL$MCLPaid
MCL$MCLIncurred

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
