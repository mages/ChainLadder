\name{BootChainLadder}
\alias{BootChainLadder}
\title{ Bootstrap-Chain-Ladder Model }
\description{
  The \code{BootChainLadder} procedure provides a predictive
  distribution of reserves or IBNRs for a cumulative claims development triangle.
}
\usage{
BootChainLadder(Triangle, R = 999, process.distr=c("gamma", "od.pois"), seed = NULL)
}

\arguments{
  \item{Triangle}{cumulative claims triangle. Assume columns are the development
	period, use transpose otherwise.  A (mxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \le n+1-i; i=1,\ldots,m; m\ge n }. See
    \code{\link{qpaid}} for how to use (mxn)-development triangles with
    m<n, say higher development period frequency (e.g quarterly) than
    origin period frequency (e.g accident years).}
  \item{R}{the number of bootstrap replicates.}
  \item{process.distr}{ character string indicating which process
    distribution to be assumed. One of "gamma" (default), 
    or "od.pois" (over-dispersed Poisson), can be abbreviated}
  \item{seed}{optional seed for the random generator}
}
\details{
  The \code{BootChainLadder} function uses a two-stage
  bootstrapping/simulation approach. In the first stage an ordinary
  chain-ladder methods is applied to the cumulative claims triangle.
  From this we calculate the scaled Pearson residuals which we bootstrap
  R times to forecast future incremental claims payments via the
  standard chain-ladder method. 
  In the second stage we simulate the process error with the bootstrap
  value as the mean and using the process distribution assumed. 
  The set of reserves obtained in this way forms the predictive distribution, 
  from which summary statistics such as mean, prediction error or
  quantiles can be derived.
}

\value{
 BootChainLadder gives a list with the following elements back:
  \item{call}{matched call}
  \item{Triangle}{input triangle}
  \item{f}{chain-ladder factors}
  \item{simClaims}{array of dimension \code{c(m,n,R)} with the simulated claims}	      
  \item{IBNR.ByOrigin}{array of dimension \code{c(m,1,R)} with the modeled
    IBNRs by origin period}
  \item{IBNR.Triangles}{array of dimension \code{c(m,n,R)} with the modeled
    IBNR development triangles}
  \item{IBNR.Totals}{vector of R samples of the total IBNRs}
  \item{ChainLadder.Residuals}{adjusted Pearson chain-ladder residuals}
  \item{process.distr}{assumed process distribution}
  \item{R}{the number of bootstrap replicates}
}
\references{
  \cite{England, PD and Verrall, RJ. Stochastic Claims Reserving in
    General Insurance (with discussion), \emph{British Actuarial Journal} 8,
    III. 2002 }
  
  \cite{Barnett and Zehnwirth. The need for diagnostic assessment of
    bootstrap predictive models, \emph{Insureware technical report}. 2007}
}
\author{ Markus Gesmann, \email{markus.gesmann@gmail.com} }
\note{ The implementation of \code{BootChainLadder} follows closely the
  discussion of the bootstrap model in section 8 and appendix 3 of the
  paper by England and Verrall (2002).
}
\seealso{ See also  
\code{\link{summary.BootChainLadder}}, 
\code{\link{plot.BootChainLadder}} displaying results and finally 
  \code{\link{CDR.BootChainLadder}} for the one year claims development result. 
}

\examples{
# See also the example in section 8 of England & Verrall (2002) on page 55.

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
fit <- fitdistr(B$IBNR.Totals[B$IBNR.Totals>0], "lognormal")
fit
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]), col="red", add=TRUE)

# See also the ABC example in  Barnett and Zehnwirth (2007) 
A <- BootChainLadder(ABC, R=999, process.distr="gamma")
A
plot(A, log=TRUE)

## One year claims development result
CDR(A)

}
\keyword{ models }
