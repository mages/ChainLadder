\name{BootChainLadder}
\alias{BootChainLadder}
%\alias{summary.BootChainLadder}
%\alias{print.BootChainLadder}
\alias{plot.BootChainLadder}
%\alias{quantile.BootChainLadder}
%\alias{residuals.BootChainLadder}
%\alias{mean.BootChainLadder}

\title{ Bootstrap-Chain-Ladder Model }
\description{
  The \code{BootChainLadder} procedure provides a predictive
  distribution of reserves for a cumulative claims development triangle.
}
\usage{
BootChainLadder(Triangle, R = 999, process.distr=c("gamma", "od.pois"))

\method{summary}{BootChainLadder}(object, probs=c(0.75,0.95), \dots)

\method{print}{BootChainLadder}(x, probs=c(0.75,0.95), \dots)

\method{plot}{BootChainLadder}(x, mfrow=c(2,2), title=NULL, log=FALSE, \dots)

\method{residuals}{BootChainLadder}(object, \dots)

\method{quantile}{BootChainLadder}(x, probs=c(0.75, 0.95), na.rm = FALSE,
              names = TRUE, type = 7,\dots)

\method{mean}{BootChainLadder}(x, \dots)

}

\arguments{
  \item{Triangle}{cumulative claims triangle.  A (mxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i, i=1,\ldots,m, m>=n }, see
    \code{\link{qpaid}} for how to use (mxn)-development triangles with
    m<n, say higher development period frequency (e.g quarterly) than
    origin period frequency (e.g accident years).}
  \item{R}{the number of bootstrap replicates.}
  \item{process.distr}{ character string indicating which process
    distributionto be assumed. One of "gamma" (default), 
    or "od.pois" (overdispersed Poisson), can be abbreviated}
  \item{x}{output from BootChainLadder}
  \item{object}{output from BootChainLadder}
  \item{mfrow}{see \code{\link{par}}}
  \item{title}{see \code{\link{title}}} 
  \item{log}{logical. If \code{TRUE} the y-axes of the 'latest
    incremental actual vs. simulated' plot will be on a log-scale}
    \item{probs}{numeric vector of probabilities with values in [0,1],
      see \code{\link{quantile}} for more help}
    \item{na.rm}{logical; if true, any \code{NA} and \code{NaN}'s are
      removed from 'x' before the quantiles are computed,
      see \code{\link{quantile}} for more help}
    \item{names}{logical; if true, the result has a \code{names}
      attribute. Set to \code{FALSE} for speedup with many 'probs',
      see \code{\link{quantile}} for more help}
 \item{type}{an integer between 1 and 9 selecting one of the nine quantile
   algorithms detailed below to be used, see \code{\link{quantile}}}         
 \item{\dots}{further arguments passed to or from other methods}
}
\details{
  The \code{BootChainLadder} function uses a two-stage
  bootstrapping/simulation approach. In the first stage an ordinary
  chain-ladder methods is applied to the cumulative claims triangle.
  From this we calculate the scaled Pearson residuals which we bootstrap
  R times to forecasts future incremental claims payements via the
  standard chain-ladder method. 
  In the second stage we simulate the process error with the bootstrap
  value as the mean and using the process distribution asumed. 
  The set of reserves obtained in this way forms the predictive distribution, 
  from which summary statistics such as mean, prediction error or
  percentiles can be derived.
}

\value{
 BootChainLadder gives a list with the following elements back:
  \item{call}{the matched call}
  \item{Triangle}{input triangle}
  \item{f}{chain-ladder factors}
  \item{simClaims}{array of dimension \code{c(m,1,R)} with the simulated claims}	      
  \item{IBNR.ByOrigin}{array of dimension \code{c(m,1,R)} with the modeled
    IBNRs by origin period}
  \item{IBNR.Triangles}{array fo dimension \code{c(m,n,R)} with the modeled
    IBNR development triangles}
  \item{IBNR.Totals}{vector of R samples of the total IBNRs}
  \item{ChainLadder.Residuals}{adjusted Pearson chain-ladder residuals}
  \item{process.distr}{assumed process distribution}
  \item{R}{number of bootstraps}

summary.BootChainLadder gives a list with the following elements back:

 \item{ByOrigin}{a data frame with summary statistics by origin period}
 \item{Totals}{a data frame with total summary statistics for all origin period}
}
\references{
  \cite{England, PD and Verrall, RJ. Stochastic Claims Reserving in
    General Insurance (with discussion), British Actuarial Journal 8,
    III. 2002 }
  }
\author{ Markus Gesmann, \email{markus.gesmann@gmail.com} }
\note{ The implimentation of \code{BootChainLadder} follows closely the
  discussion of the bootstrap model in section 8 and appendix 3 of the
  above paper by England and Verall.

  The box-plots follow the ideas of B. Zehnwirth and G. Barnett in:
  \cite{Barnett and Zehnwirth. The need for diagnostic assessment of
    bootstrap predictive models, Insureware technical report. 2007}
}
\seealso{ See also  \code{\link{MackChainLadder}} }
\examples{
# See as well the example in section 8 of England & Verrall's paper on page 55.
B <- BootChainLadder(RAA, R=999, process.distr="gamma")
B
plot(B)
# Compare to MackChainLadder
MackChainLadder(RAA)
quantile(B, c(0.75,0.95,0.99, 0.995))

# fit a distribution to the IBNR
library(MASS)
plot(ecdf(B$IBNR.Totals))
# fit a log-normal distribution 
fit <- fitdistr(B$IBNR.Totals, "lognormal")
fit
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]), col="red", add=TRUE)

# See as well the ABC example in Zehnwirth and Barnett paper
A <- BootChainLadder(ABC, R=999, process.distr="gamma")
A
plot(A, log=TRUE)



}
\keyword{ models }
