\name{BootChainLadder}
\alias{BootChainLadder}
\alias{summary.BootChainLadder}
\alias{print.BootChainLadder}
\alias{plot.BootChainLadder}
\alias{quantile.BootChainLadder}

\title{ A bootstrap aproach to estimate the distribution of IBNR claims. }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BootChainLadder(Triangle = RAA, R = 999, process.distr = "gamma")
\method{summary}{BootChainLadder}(object,probs=c(0.75,0.99), \dots)

\method{print}{BootChainLadder}(x,probs=c(0.75,0.99), \dots)

\method{plot}{BootChainLadder}(x,mfrow=c(1,2),title=NULL, \dots)

\method{quantile}{BootChainLadder}(x,probs=c(0.75, 0.99), na.rm = FALSE,
              names = TRUE, type = 7,\dots)



}

\arguments{
  \item{Triangle}{a cumulative claims triangle.  A quadratic (nxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i, i=1,\ldots,n },
    see \code{\link{qpaid}} for how to use (mxn)-development triangles with m<n. }
  \item{R}{the number of bootstrap replicates. }
  \item{process.distr}{ a string of (currently) either "gamma" or
    "od.pois" to model the process distribution. }
  \item{x}{output from BootChainLadder}
  \item{object}{output from BootChainLadder}
  \item{mfrow}{see \code{\link{par}}}
  \item{title}{see \code{\link{title}}} 
  \item{probs}{numeric vector of probabilities with values in [0,1], see \code{\link{quantile}}}
  \item{na.rm}{logical; if true, any \code{NA} and \code{NaN}'s are removed from 'x'
          before the quantiles are computed, see \code{\link{quantile}}}
  \item{names}{logical; if true, the result has a \code{names} attribute.  Set to
          \code{FALSE} for speedup with many 'probs', see \code{\link{quantile}}}
 \item{type}{an integer between 1 and 9 selecting one of the nine quantile
          algorithms detailed below to be used, see \code{\link{quantile}}}         
  \item{\dots}{further arguments passed to or from other methods}


}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
 
  \item{call}{the matched call}
  \item{Triangle}{input triangle}
  \item{IBNR.ByOrigin}{an array of dimension m,1,R with the modeled
    IBNRs by origin period.}
  \item{IBNR.Triangles}{an array fo dimension m,n,R with the modeled
    IBNR development triangles}
  \item{IBNR.Totals}{a vector R samples of total IBNRs}
  \item{process.distr}{input process distribution}
  \item{R}{input number of bootstraps}

 
}
\references{ England, PD and Verrall, RJ (2002). Stochastic Claims Reserving in General Insurance (with discussion), British Actuarial Journal 8, III }
\author{ Markus Gesmann, \email{markus.gesmann@gmail.com} }
\note{ The implimentation of BootChainLadder() follows closely the discussion of the
  bootstrap model in appendix 3 of the above paper by England and Verall.
}
\seealso{ See also  \code{\link{MackChainLadder}} }
\examples{
B <- BootChainLadder(RAA, R=99, process.distr="gamma")
B
# Compare to MackChainLadder
MackChainLadder(RAA)
quantile(B, c(0.75,0.9,0.99, 0.995))

# fitdistribution
library(MASS)
plot(ecdf(B$IBNR.Totals))
fit <-  fitdistr(B$IBNR.Totals, "lognormal")
fit
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]), col="red", add=TRUE)






C <- BootChainLadder(RAA, R=99, process.distr="od.pois")
}
\keyword{ models }
