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
\method{summary}{BootChainLadder}(object, probs=c(0.75, 0.99), \dots)

\method{print}{BootChainLadder}(x, \dots)

\method{quantile}{BootChainLadder}(x, \dots)

\method{plot}{BootChainLadder}(x, mfrow=c(1,2), title=NULL, \dots)

}

\arguments{
  \item{Triangle}{a cumulative claims triangle.  A quadratic (nxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i, i=1,\ldots,n },
    see \code{\link{qpaid}} for how to use (mxn)-development triangles with m<n. }
  \item{R}{the number of bootstrap replicates. }
  \item{process.distr}{ a string of (currently) either "gamma" or
    "od.pois" to model the process distribution. }
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
MackChainLadder(B)
quantile(B, c(0.75,0.9,0.99, 0.995))

C <- BootChainLadder(RAA, R=99, process.distr="od.pois")
}
\keyword{ models }
