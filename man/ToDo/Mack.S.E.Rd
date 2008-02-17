\name{Mack.S.E}
\alias{Mack.S.E}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Mack.S.E(MackModel, FullTriangle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MackModel}{ ~~Describe \code{MackModel} here~~ }
  \item{FullTriangle}{ ~~Describe \code{FullTriangle} here~~ }
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
function (MackModel, FullTriangle) 
{
    n <- ncol(FullTriangle)
    m <- nrow(FullTriangle)
    f <- rep(1, n)
    f.se <- rep(0, n)
    sigma <- rep(0, (n - 1))
    f[1:(n - 1)] <- sapply(MackModel, function(x) summary(x)$coef["x", 
        "Estimate"])
    f.se[1:(n - 1)] <- sapply(MackModel, function(x) summary(x)$coef["x", 
        "Std. Error"])
    sigma[1:(n - 1)] <- sapply(MackModel, function(x) summary(x)$sigma)
    dev = c(1:(n - 2))
    sigmaModel <- lm(log(sigma[-(n - 1)]) ~ dev)
    sigma[n - 1] <- exp(predict(sigmaModel, newdata = data.frame(dev = (n - 
        1))))
    f.se[n - 1] = sigma[n - 1]/sqrt(FullTriangle[1, n - 1])
    F.se <- t(t(1/sqrt(FullTriangle)[, -n]) * (sigma))
    FullTriangle.se <- FullTriangle * 0
    for (i in 2:m) {
        for (k in c((n + 1 - i):(n - 1))) {
            FullTriangle.se[i, k + 1] = sqrt(FullTriangle[i, 
                k]^2 * (F.se[i, k]^2 + f.se[k]^2) + FullTriangle.se[i, 
                k]^2 * f[k]^2)
        }
    }
    return(list(sigma = sigma, f = f, f.se = f.se, FullTriangle.se = FullTriangle.se))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
