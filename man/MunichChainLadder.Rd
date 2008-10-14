\name{MunichChainLadder}
\alias{MunichChainLadder}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Munich-Chain-Ladder Model }
\description{
  The Munich-Chain-Ladder model forecasts IBNR claims based on a cumulative
  paid and incurred claims triangle. 
  The Munich-Chain-Ladder model assumes that the Mack-model is applicable
  to the paid and incurred claims triangle, see \code{\link{MackChainLadder}}.
}
\usage{
MunichChainLadder(Paid, Incurred, 
                  est.sigmaP = "log-linear", est.sigmaI = "log-linear", 
                  tailP=FALSE, tailI=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Paid}{cumulative paid claims triangle. A (mxn)-matrix \eqn{P_{ik}} 
    which is filled for \eqn{k \leq n+1-i; i=1,\ldots,m; m\geq n}}
  \item{Incurred}{cumulative incurred claims triangle. A (mxn)-matrix
    \eqn{I_{ik}} which is filled for \eqn{k \leq n+1-i; i=1,\ldots,m,
      m\geq n }}
  \item{est.sigmaP}{defines how \eqn{sigma_{n-1}} for the Paid triangle
    is estimated, see \code{est.sigma} in \code{\link{MackChainLadder}} for
    more details, as \code{est.sigmaP} gets passed on to
    \code{MackChainLadder}
  }
  \item{est.sigmaI}{defines how \eqn{sigma_{n-1}} for the Incurred triangle
    is estimated, see \code{est.sigma} in \code{\link{MackChainLadder}} for
    more details, as \code{est.sigmaI} is passed on to
    \code{MackChainLadder}
  }
  \item{tailP}{defines how the tail of the \code{Paid} triangle is estimated and
    is passed on to \code{\link{MackChainLadder}}, see \code{tail} just there.}
  \item{tailI}{defines how the tail of the \code{Incurred} triangle is estimated and
    is passed on to \code{\link{MackChainLadder}}, see \code{tail} just there.}
}
%\details{
% 
%}
\value{
  MunichChainLadder returns a list with the following elements
  \item{call}{matched call} 
  \item{Paid}{input paid triangle}
  \item{Incurred}{input incurred triangle}
  \item{MCLPaid}{Munich-chain-ladder forecasted full triangle on paid data}
  \item{MCLIncurred}{Munich-chain-ladder forecasted full triangle on incurred data}
  \item{MackPaid}{Mack-chain-ladder output of the paid triangle}
  \item{MackIncurred}{Mack-chain-ladder output of the incurred triangle}
  \item{PaidResiduals}{paid residuals}
  \item{IncurredResiduals}{incurred residuals}
  \item{QResiduals}{paid/incurred residuals}
  \item{QinverseResiduals}{incurred/paid residuals}
  \item{lambdaP}{dependency coefficient between paid chain ladder ratios and
    incurred/paid ratios}
  \item{lambdaI}{dependency coefficient between incurred chain ladder
    ratios and paid/incurred ratios}
  \item{qinverse.f}{chain-ladder factors of the incurred/paid triangle}
  \item{rhoP.sigma}{estimation of the conditional deviation around the
    paid/incurred ratios}
  \item{q.f}{chain-ladder factors of the paid/incurred triangle}
  \item{rhoI.sigma}{estimation of the conditional deviation around the
    incurred/paid ratios}
}
\references{
  \cite{Gerhard Quarg and Thomas Mack. Munich Chain Ladder. \emph{Blatter DGVFM} 26,
  Munich, 2004.}
  }
\author{ Markus Gesmann \email{markus.gesmann@gmail.com} }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See also \code{\link{MackChainLadder}}, 
  \code{\link{summary.MunichChainLadder}}, 
  \code{\link{plot.MunichChainLadder}} }
\examples{

MCLpaid
MCLincurred

M <- MunichChainLadder(MCLpaid, MCLincurred)
M
plot(M)
# You can access the standard chain ladder (Mack) output via
M$MackPaid
M$MackIncurred

# Following the example output in Quarg's paper:
MCL <- MunichChainLadder(MCLpaid, MCLincurred, est.sigmaP=0.1, est.sigmaI=0.1)
# Input triangles section 3.3.1
MCL$Paid
MCL$Incurred
# Parameters from section 3.3.2
# Standard chain ladder factors
MCL$MackPaid$f
MCL$MackIncurred$f
MCL$MackPaid$sigma
MCL$MackIncurred$sigma
# Check Mack's assumptions graphically
plot(MCL$MackPaid)
plot(MCL$MackIncurred)

MCL$q.f
MCL$rhoP.sigma
MCL$rhoI.sigma

MCL$PaidResiduals
MCL$IncurredResiduals

MCL$QinverseResiduals
MCL$QResiduals

MCL$lambdaP
MCL$lambdaI
# Section 3.3.3 Results
MCL$MCLPaid
MCL$MCLIncurred

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
