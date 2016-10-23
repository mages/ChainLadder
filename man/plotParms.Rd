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
Methods to visualize the estimated parameters. 
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
defaults to something the class author deems appropriate.
  }
  \item{\dots}{arguments passed to other methods}
}
\details{
Currently six plots are available
\itemize{
  \item{1}{estimated link ratios f}
  \item{2}{estimated link ratios f less unity}
  \item{3}{estimated sigma values}
  \item{4}{estimated standard error of the link ratio f.se}
  \item{5}{coefficient of variation of the link ratio}
  \item{6}{coefficient of variation of the link ratio less unity}
}
}
\value{
In the case of class ChainLadder or MackChainLadder,
list of class arrangelist returned by 
gridExtra::marrangeGrob.
In all other cases, a stop error is executed at this time.
}
\author{
dmm
}
\examples{
plotParms(chainladder(GenIns))
plotParms(MackChainLadder(GenIns))
}
\keyword{ models }
