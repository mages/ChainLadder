\name{plot.BootChainLadder}
\alias{plot.BootChainLadder}
\title{ Plot method for a BootChainLadder object}
\description{
  \code{plot.BootChainLadder}, a method to plot the output of
  \code{\link{BootChainLadder}}. It is designed to give a quick look at a
  \code{BootChainLadder} object, to check the result and the model assumptions.
}
\usage{
\method{plot}{BootChainLadder}(x, mfrow=c(2,2), title=NULL, log=FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{output from \code{BootChainLadder}}
  \item{mfrow}{see \code{\link{par}}}
  \item{title}{see \code{\link{title}}} 
  \item{log}{logical. If \code{TRUE} the y-axes of the 'latest
    incremental actual vs. simulated' plot will be on a log-scale}
  \item{\dots}{ optional arguments to \code{plot} method }
}
\details{
 \code{plot.BootChainLadder} shows four graphs, staring with a histogram
 of the total simulated IBNRs over all origin periods, including a rug
 plot; a plot of the empirical cumulative distribution of the total
 IBNRs over all origin periods; a box-whisker plot of simulated ultimate
 claims costs against origin periods; and a box-whisker plot of
 simulated incremental claims cost for the latest available calendar period
 against actual incremental claims in the same period. In the last plot
 the simulated data should follow the same trend as the actual,
 otherwise the original data might have some intrinsic trends which are
 not reflected in the model.
}
\author{Markus Gesmann}
\note{
  The box-plot of latest actual incremental claims against simulated
  claims follows ideas of B. Zehnwirth and G. Barnett in:
  \cite{Barnett and Zehnwirth. \emp{The need for diagnostic assessment of
    bootstrap predictive models}, Insureware technical report. 2007}
}
\seealso{ See Also \code{\link{BootChainLadder}} }
\examples{
B <- BootChainLadder(RAA)
plot(B)
}
\keyword{ aplot }

