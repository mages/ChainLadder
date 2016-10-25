\name{plotParms}
\alias{plotParms}
\alias{plotParms.MackChainLadder}
\alias{plotParms.ChainLadder}
\alias{plotParms.default}
\title{
Plot the estimated parameters of a model
}

\encoding{UTF-8}

\description{
Generic function for visualizing the estimated parameters
of a model in the ChainLadder package.
S3 methods currently exist for objects of class
ChainLadder and MackChainLadder.
}

\usage{
plotParms(x, which, ncol, nrow, title, \dots)
\method{plotParms}{ChainLadder}(x, which = c(1, 3, 4, 6), ncol, nrow, title, \dots)
\method{plotParms}{MackChainLadder}(x, which = c(1, 3, 4, 6), ncol, nrow, title, \dots)
\method{plotParms}{default}(x, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{object whose parameters are to be visualized}
  \item{which}{if a subset of the plots is required,
  specify a subset of the numbers 1:6}
  \item{ncol}{number of columns in the resulting plot}
  \item{nrow}{number of rows in the resulting plot}
  \item{title}{optional; character holding title of the plot;
defaults to method call.
  }
  \item{\dots}{arguments passed to other methods}
}
\details{
Currently six plots are available for 
\code{chainladder} and \code{MackChainLadder}:
\enumerate{
  \item {estimated link ratios f}
  \item {estimated link ratios f less unity}
  \item {estimated sigma values}
  \item {estimated standard error of the link ratio f.se}
  \item {coefficient of variation of the link ratio}
  \item {coefficient of variation of the link ratio less unity}
}
}
\value{
In the case of class ChainLadder or MackChainLadder,
list of class \code{arrangelist} returned by 
\code{gridExtra::marrangeGrob}.
In all other cases, a \code{stop} error is executed at this time.
}
\author{
dmm
}
\examples{
plotParms(chainladder(GenIns))
plotParms(MackChainLadder(GenIns))
}
\keyword{ models }
