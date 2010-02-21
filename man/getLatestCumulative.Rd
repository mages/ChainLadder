\name{getLatestCumulative}
\alias{getLatestCumulative}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Triangle information for most recent calendar period.
}
\description{
  Return most recent values for all origin periods of a cumulative development triangle.
}
\usage{
getLatestCumulative(cumulative.tri)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cumulative.tri}{a cumulative triangle. Assume columns are the development
    period, use transpose otherwise.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A vector of most recent non-'NA' values of a triangle for all origin periods.  
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%Ben Escoto, Markus Gesmann  
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{as.triangle}}.
}
\examples{
RAA
getLatestCumulative(RAA)
}
\keyword{ methods }
