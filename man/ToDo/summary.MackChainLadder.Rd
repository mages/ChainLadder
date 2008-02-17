\name{summary.MackChainLadder}
\alias{summary.MackChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.MackChainLadder(MackModel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MackModel}{ ~~Describe \code{MackModel} here~~ }
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
function (MackModel) 
{
    .Triangle <- MackModel[["Triangle"]]
    n <- ncol(.Triangle)
    m <- nrow(.Triangle)
    Latest <- rev(.Triangle[row(as.matrix(.Triangle)) == (m + 
        1 - col(as.matrix(.Triangle)))])
    Ultimate <- MackModel[["FullTriangle"]][, n]
    Dev.To.Date <- Latest/Ultimate
    Reserve <- Ultimate - Latest
    Mack.S.E <- MackModel[["Mack.S.E"]][, n]
    Mack.S.E.Ratio <- Mack.S.E/(Ultimate - Latest)
    myResult <- data.frame(Latest, Dev.To.Date, Ultimate, Reserve, 
        Mack.S.E, Mack.S.E.Ratio)
    return(myResult)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
