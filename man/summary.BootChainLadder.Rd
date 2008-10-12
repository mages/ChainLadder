\name{summary.BootChainLadder}
\alias{summary.BootChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.BootChainLadder(object, probs = c(0.75, 0.95), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ ~~Describe \code{object} here~~ }
  \item{probs}{ ~~Describe \code{probs} here~~ }
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
function(object,probs=c(0.75,0.95),...){

    .Triangle <- object$Triangle
    m <- dim(.Triangle)[1]
    n <- dim(.Triangle)[2]

    dim(.Triangle) <- c(dim(.Triangle),1)
    Latest <- as.vector(getLatest(getIncremental(.Triangle)))


    IBNR <- object$IBNR.ByOrigin
    dim(IBNR) <- dim(IBNR)[c(1,3)]
    IBNR.q <- apply(IBNR, 1, quantile, probs=probs,...)
    IBNR.mean <- apply(IBNR, 1, mean)
    IBNR.sd <- apply(IBNR, 1, sd)
    sumIBNR <- t(rbind(IBNR.mean, IBNR.sd, IBNR.q))
    sumIBNR <- as.data.frame(sumIBNR)

    ## ByOrigin
    ByOrigin <- data.frame(Latest,
                           Ult.mean=Latest+sumIBNR$IBNR.mean,
                           sumIBNR)
    names(ByOrigin) <- c("Latest", "Mean Ultimate", "Mean IBNR",
                         "SD IBNR", paste("IBNR ", probs*100, "\%", sep=""))
    ex.origin.period <- Latest!=0
    ByOrigin <- ByOrigin[ex.origin.period,]

    origin <- dimnames(object$Triangle)[[1]]
    if(length(origin)==nrow(ByOrigin)){
        rownames(ByOrigin) <- origin
    }

    ## Totals
    Total.Latest <- sum(Latest,na.rm=TRUE)
    Total.IBNR <- object$IBNR.Totals
    Total.IBNR.mean <-  mean(Total.IBNR)
    Total.IBNR.sd <-  sd(Total.IBNR)
    Total.IBNR.q <- quantile(Total.IBNR, probs=probs,...)

    Totals <-  c(Total.Latest, Total.Latest+Total.IBNR.mean,
                 Total.IBNR.mean, Total.IBNR.sd, Total.IBNR.q)
    Totals <- as.data.frame(Totals)

    colnames(Totals)=c("Totals")
    rownames(Totals) <- c("Latest:","Mean Ultimate:",
                          "Mean IBNR:","SD IBNR:",
                          paste("Total IBNR ", probs*100, "\%:", sep="") )

    output <- list(ByOrigin=ByOrigin, Totals=Totals)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
