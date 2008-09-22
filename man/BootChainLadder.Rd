\name{BootChainLadder}
\alias{BootChainLadder}
\alias{summary.BootChainLadder}
\alias{print.BootChainLadder}
\alias{plot.BootChainLadder}
\alias{quantile.BootChainLadder}

\title{ Bootstrap-Chain-Ladder Model }
\description{
The \code{BootChainLadder} procedure provides a predictive distribution of reserves for a cumulative claims development 
triangle.
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
  \item{process.distr}{defines the process distribution, currently either "gamma" or
    "od.pois" (over-dispersed Poisson)}

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
The \code{BootChainLadder} function uses a two-stage bootstrapping/simulation approach.
In the first stage an ordinary chain-ladder methods is applied to the cumulatitve claims triangle. From this we calculate
the scaled Pearson residuals which we bootstrap R times to forecasts future incremental claims payements via the standard chain-ladder method. In the second stage we simulate the process error with the bootstrap value as the mean and using the process distribution asumed. The set of reserves obtained in this way forms the predictive 
distribution, from which summary statistics such as mean, prediction error or percentiles can be derived.


}

\value{
 BootChainLadder gives a list with the following elements back:
  \item{call}{the matched call}
  \item{Triangle}{the input triangle}
  \item{IBNR.ByOrigin}{the array of dimension \code{c(m,1,R)} with the modeled
    IBNRs by origin period.}
  \item{IBNR.Triangles}{the array fo dimension \code{c(m,n,R)} with the modeled
    IBNR development triangles}
  \item{IBNR.Totals}{the vector of R samples of the total IBNRs}
  \item{process.distr}{the name of the process distribution}
  \item{R}{the number of bootstraps}

summary.BootChainLadder gives a list with the following elements back:

 \item{ByOrigin}{a data frame with summary statistics by origin period}
 \item{Totals}{a data frame with total summary statistics for all origin period}
}
\references{ England, PD and Verrall, RJ (2002). Stochastic Claims Reserving in General Insurance (with discussion), British Actuarial Journal 8, III }
\author{ Markus Gesmann, \email{markus.gesmann@gmail.com} }
\note{ The implimentation of BootChainLadder() follows closely the discussion of the
  bootstrap model in section 8 and appendix 3 of the above paper by England and Verall.

Many thanks to Nigel de Silva for all the ideas on how to use arrays effiecently.
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
# fit a log-normal distribution 
fit <- fitdistr(B$IBNR.Totals, "lognormal")
fit
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]), col="red", add=TRUE)


}
\keyword{ models }
