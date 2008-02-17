\name{summary.MunichChainLadder}
\alias{summary.MunichChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.MunichChainLadder(MCL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MCL}{ ~~Describe \code{MCL} here~~ }
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
function (MCL) 
{
    n <- ncol(MCL[["MCLPaid"]])
    .Paid <- as.matrix(MCL[["Paid"]])
    .Incurred <- as.matrix(MCL[["Incurred"]])
    LatestPaid = rev(.Paid[row(.Paid) == (n + 1 - col(.Paid))])
    LatestIncurred = rev(.Incurred[row(.Incurred) == (n + 1 - 
        col(.Incurred))])
    UltimatePaid = MCL[["MCLPaid"]][, n]
    UltimateIncurred = MCL[["MCLIncurred"]][, n]
    Result <- data.frame(LatestPaid, LatestIncurred, Latest.P.I.Ratio = LatestPaid/LatestIncurred, 
        UltimatePaid, UltimateIncurred, Ultimate.P.I.Ratio = UltimatePaid/UltimateIncurred)
    return(Result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
