\name{plot.BootChainLadder}
\alias{plot.BootChainLadder}
\title{ Plot method for a BootChainLadder object}
\description{
  \code{plot.BootChainLadder}, a method to plot the output of
  \code{\link{BootChainLadder}}. It is designed to give a quick overview
  of a \code{BootChainLadder} object and to check the model assumptions.
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
  \item{\dots}{ optional arguments. See \code{\link{plot.default}} for
    more details. }
}
\details{
 \code{plot.BootChainLadder} shows four graphs, starting with a histogram
 of the total simulated IBNRs over all origin periods, including a rug
 plot; a plot of the empirical cumulative distribution of the total
 IBNRs over all origin periods; a box-whisker plot of simulated ultimate
 claims costs against origin periods; and a box-whisker plot of
 simulated incremental claims cost for the latest available calendar period
 against actual incremental claims of the same period. In the last plot
 the simulated data should follow the same trend as the actual data,
 otherwise the original data might have some intrinsic trends which are
 not reflected in the model.
}
\author{Markus Gesmann}
\note{
  The box-whisker plot of latest actual incremental claims against simulated
  claims follows is based on ideas from Barnett and Zehnwirth in:
  \cite{Barnett and Zehnwirth. The need for diagnostic assessment of
    bootstrap predictive models, \emph{Insureware technical report}. 2007}
}
\seealso{ See also \code{\link{BootChainLadder}} }
\examples{
B <- BootChainLadder(RAA)
plot(B)
plot(B, log=TRUE)
}
\keyword{ aplot }

