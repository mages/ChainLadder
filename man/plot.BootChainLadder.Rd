\name{plot.BootChainLadder}
\alias{plot.BootChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.BootChainLadder(x, mfrow = c(2, 2), title = NULL, log = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{mfrow}{ ~~Describe \code{mfrow} here~~ }
  \item{title}{ ~~Describe \code{title} here~~ }
  \item{log}{ ~~Describe \code{log} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,mfrow=c(2,2),title=NULL,log=FALSE,...){

    if(is.null(title)) myoma <- c(0,0,0,0) else myoma <- c(0,0,2,0)

    Total.IBNR <- x$IBNR.Total

    op=par(mfrow=mfrow, oma=myoma,...)
    ## Histogram
    hist(Total.IBNR, xlab="Total IBNR")
    lines(density(Total.IBNR))
    rug(Total.IBNR)
    ## Empirical distribution
    plot(ecdf(Total.IBNR), xlab="Total IBNR")

    ## Plot simultated Ultiamtes by Origin
    plotBootstrapUltimates(x)

    ## Backtest last developemt year and check for cal. year trends
    backTest <- backTestLatestIncremental(x)
    plotLatestIncremental(backTest, log=log)

    title( title , outer=TRUE)
    par(op)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
