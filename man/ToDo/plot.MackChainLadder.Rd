\name{plot.MackChainLadder}
\alias{plot.MackChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.MackChainLadder(x, myUnit = 1000, Currency = "£", mfrow = c(3, 2), title = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{myUnit}{ ~~Describe \code{myUnit} here~~ }
  \item{Currency}{ ~~Describe \code{Currency} here~~ }
  \item{mfrow}{ ~~Describe \code{mfrow} here~~ }
  \item{title}{ ~~Describe \code{title} here~~ }
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
function (x, myUnit = 1000, Currency = "£", mfrow = c(3, 2), 
    title = NULL, ...) 
{
    if (is.null(title)) 
        myoma <- c(0, 0, 0, 0)
    else myoma <- c(0, 0, 2, 0)
    op = par(mfrow = mfrow, oma = myoma)
    .myResult <- summary(x)
    .FullTriangle <- x[["FullTriangle"]]
    .Triangle <- x[["Triangle"]]
    n <- nrow(.Triangle)
    bp <- barplot(t(as.matrix(.myResult[, c("Latest", "Reserve")]/myUnit)), 
        legend.text = c("Latest", "Reserve"), names.arg = c(1:n), 
        main = "Mack Chain Ladder Results", xlab = "Origin year", 
        ylab = paste(Currency, myUnit), ylim = c(0, max(.myResult$Ultimate + 
            .myResult$Mack.S.E)/myUnit), ...)
    library(Hmisc)
    errbar(x = bp, y = .myResult$Ultimate/myUnit, yplus = (.myResult$Ultimate + 
        .myResult$Mack.S.E)/myUnit, yminus = (.myResult$Ultimate - 
        .myResult$Mack.S.E)/myUnit, cap = 0.05, add = TRUE)
    matplot(t(.FullTriangle)/myUnit, t = "l", main = "Chain ladder developments by origin year", 
        xlab = "Development year", ylab = paste(Currency, myUnit))
    matplot(t(.Triangle)/myUnit, add = TRUE)
    Residuals = residuals(x)
    attach(Residuals)
    plot(standard.residuals ~ fitted.value, ylab = "Standardised residuals", 
        xlab = "Fitted")
    lines(lowess(fitted.value, standard.residuals), col = "red")
    abline(h = 0, col = "grey")
    plot(standard.residuals ~ origin.year, ylab = "Standardised residuals", 
        xlab = "Origin year")
    lines(lowess(origin.year, standard.residuals), col = "red")
    abline(h = 0, col = "grey")
    plot(standard.residuals ~ cal.year, ylab = "Standardised residuals", 
        xlab = "Calendar year")
    lines(lowess(cal.year, standard.residuals), col = "red")
    abline(h = 0, col = "grey")
    plot(standard.residuals ~ dev.year, ylab = "Standardised residuals", 
        xlab = "Development year")
    lines(lowess(dev.year, standard.residuals), col = "red")
    abline(h = 0, col = "grey")
    detach(Residuals)
    title(title, outer = TRUE)
    par(op)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
