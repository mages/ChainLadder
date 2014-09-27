\name{CDR.BootChainLadder}
\alias{CDR.BootChainLadder}

\title{
Claims Development Result for BootChainLadder
}
\description{
Estimate the Claims Development Result for BootChainLadder 
output.
}
\usage{
CDR.BootChainLadder(B, probs = 0.995, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{B}{output of \code{BootChainLadder}}
  \item{probs}{vector of quantiles to be estimated
}
  \item{\dots}{currently not used.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{A \code{data.frame} with summary statistics.}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Giuseppe Crupi and Markus Gesmann
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{BootChainLadder}}
}
\examples{
B <- BootChainLadder(MWR)
B
CDR.BootChainLadder(B)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ model }
