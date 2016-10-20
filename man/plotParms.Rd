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
plotParms(x, \dots)
\method{plotParms}{ChainLadder}(x, title, \dots)
\method{plotParms}{MackChainLadder}(x, title, \dots)
\method{plotParms}{default}(x, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{object whose parameters are to be visualized}
  \item{title}{optional; character holding title of the plot;
defaults to something the class author deems appropriate.
  }
  \item{\dots}{unused at this time}
}
\details{
TBD
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
