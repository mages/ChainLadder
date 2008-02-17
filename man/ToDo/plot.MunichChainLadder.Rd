\name{plot.MunichChainLadder}
\alias{plot.MunichChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.MunichChainLadder(x, myUnit = 1000, mfrow = c(2, 2), Currency = "£")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{myUnit}{ ~~Describe \code{myUnit} here~~ }
  \item{mfrow}{ ~~Describe \code{mfrow} here~~ }
  \item{Currency}{ ~~Describe \code{Currency} here~~ }
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
function (x, myUnit = 1000, mfrow = c(2, 2), Currency = "£") 
{
    op = par(mfrow = mfrow)
    n <- ncol(x[["MCLPaid"]])
    barplot(t(as.matrix(data.frame(Paid = x[["MCLPaid"]][, n]/myUnit, 
        Incurred = x[["MCLIncurred"]][, n]/myUnit))), beside = TRUE, 
        legend.text = c("MCL Paid", "MCL Incurred"), names.arg = c(1:n), 
        xlab = "origin year", ylab = paste(Currency, myUnit), 
        main = "Munich Chain Ladder Results")
    barplot(t(as.matrix(data.frame(SCL.PI = 100 * x[["MackPaid"]]$FullTriangle[, 
        n]/x[["MackIncurred"]]$FullTriangle[, n], MCL.PI = 100 * 
        x[["MCLPaid"]][, n]/x[["MCLIncurred"]][, n]))), beside = TRUE, 
        legend.text = c("SCL P/I", "MCL P/I"), names.arg = c(1:n), 
        xlab = "origin year", ylab = "\%", main = "Munich Chain Ladder Results")
    plot(x[["PaidResiduals"]][left.tri(x[["PaidResiduals"]])] ~ 
        x[["QinverseResiduals"]][left.tri(x[["QinverseResiduals"]])], 
        xlim = c(-2, 2), ylim = c(-2, 2), xlab = "Paid residuals", 
        ylab = "Incurred/Paid residuals", main = "Paid residual plot")
    abline(v = 0)
    abline(h = 0)
    abline(a = 0, b = x[["lambdaP"]], col = "red")
    plot(x[["IncurredResiduals"]][left.tri(x[["IncurredResiduals"]])] ~ 
        x[["QResiduals"]][left.tri(x[["QResiduals"]])], xlim = c(-2, 
        2), ylim = c(-2, 2), xlab = "Incurred residuals", ylab = "Paid/Incurred residuals", 
        main = "Incurred residual plot")
    abline(v = 0)
    abline(h = 0)
    abline(a = 0, b = x[["lambdaI"]], col = "red")
    par(op)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
