\name{ChainLadder}
\Rdversion{1.1}
\alias{chainladder}
\title{ Estimate age-to-age factors }
\description{
Basic chain ladder function to estimate age-to-age factors for a given
cumulative run-off triangle. This function is used by Mack- and MunicChainLadder.
}
\usage{
chainladder(Triangle, weights = 1, delta = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{Triangle}{cumulative claims triangle.  A (mxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i; i=1,\ldots,m; m\geq n }, see
    \code{\link{qpaid}} for how to use (mxn)-development triangles with
    m<n, say higher development period frequency (e.g quarterly) than
    origin period frequency (e.g accident years).}
  \item{weights}{ weights. Default: 1, which sets the weights for all
    triangle entries to 1. Otherwise specify weights as a matrix of the same
    dimension as \code{Triangle} with all weight entries in [0; 1]}
  \item{delta}{'weighting' parameters, either 0,1 or 2. Default: 1.
    See Zehnwirth & Barnett (2000); delta=1 gives the historical chain
    ladder age-to-age factors, delta=0 gives the straight average of the
    observed individual development factors and delta=2 is the result of
    an ordinary regression of \eqn{C_{i,k+1}} against \eqn{C_{i,k}} with
    intercept 0.
    Please note that Mack (1999) used the notation of alphas, with alpha=2-delta.
  }
}

\details{
 
  The key idea is to see the chain ladder algorithm as a weighted linear model
  through the origin applied to each development period.

  Say \code{y} are all cummulative claims at development period
  \code{i+1}, and \code{x} at development period \code{i}, \code{w} are
  weighting factors and \code{F} the individual age-to-age factors \code{F=y/x}, than
  we get the various age-to-age factors for different deltas (alphas) as:
  \code{
  sum(w*x^alpha*F)/sum(w*x^alpha) # Mack's (1999) notation
  delta <- 2-alpha
  lm(y~x + 0 ,weights=w/x^delta) # Zehnwirth & Barnett (2000) notation
  }

  
 }
\value{
  \code{chainladder} returns a list with the following elements
  \item{Models}{linear regression models for each development period}
  \item{Triangle}{input triangle of cumulative claims}
  \item{weights}{weights used}
  \item{delta}{deltas used}
}
\references{
  
\cite{Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. \emph{Astin Bulletin}. Vol. 29. No 2. 1999. pp.361:366}

\cite{B. Zehnwirth and G. Barnett. Best Estimates for
  Reserves. \emph{Proceedings of the CAS.} Volume LXXXVII. Number 167. November 2000.}

}
\author{
  Markus Gesmann <markus.gesmann@gmail.com>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{See also
 \code{\link{MackChainLadder}}, 
 \code{\link{predict.ChainLadder}}
}
\examples{

## Concept of different chain ladder age-to-age factors
x <- RAA[1:9,1]
y <- RAA[1:9,2]

weights <- RAA
weights[!is.na(weights)] <- 1
w <- weights[1:9,1]

F <- y/x
## wtd. average chain ladder age-to-age factors
alpha <- 1
delta <- 2-alpha

sum(w*x^alpha*F)/sum(w*x^alpha)
lm(y~x + 0 ,weights=w/x^delta)
summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef

## straight average age-to-age factors
alpha <- 0
delta <- 2 - alpha 
sum(w*x^alpha*F)/sum(w*x^alpha)
lm(y~x + 0 ,weights=w/x^(2-alpha))
summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef

## regression age-to-age factors
alpha=2
delta <- 2-alpha
sum(w*x^alpha*F)/sum(w*x^alpha)
lm(y~x + 0 ,weights=w/x^delta)
summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef

## Change weights

weights[2,1] <- 0.5
w <- weights[1:9,1] 

## wtd. average chain ladder age-to-age factors
alpha <- 1
delta <- 2-alpha
sum(w*x^alpha*F)/sum(w*x^alpha)
lm(y~x + 0 ,weights=w/x^delta)
summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef

## straight average age-to-age factors
alpha <- 0
delta <- 2 - alpha 
sum(w*x^alpha*F)/sum(w*x^alpha)
lm(y~x + 0 ,weights=w/x^(2-alpha))
summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef

## regression age-to-age factors
alpha=2
delta <- 2-alpha
sum(w*x^alpha*F)/sum(w*x^alpha)
lm(y~x + 0 ,weights=w/x^delta)
summary(chainladder(RAA, weights=weights, delta=delta)$Models[[1]])$coef

## Model review
CL0 <- chainladder(RAA, weights=weights, delta=0)
## age-to-age factors
sapply(CL0$Models, function(x) summary(x)$coef["x","Estimate"])
## f.se
sapply(CL0$Models, function(x) summary(x)$coef["x","Std. Error"])
## sigma
sapply(CL0$Models, function(x) summary(x)$sigma)

CL1 <- chainladder(RAA, weights=weights, delta=1)
## age-to-age factors
sapply(CL1$Models, function(x) summary(x)$coef["x","Estimate"])
## f.se
sapply(CL1$Models, function(x) summary(x)$coef["x","Std. Error"])
## sigma
sapply(CL1$Models, function(x) summary(x)$sigma)

CL2 <- chainladder(RAA, weights=weights, delta=2)
## age-to-age factors
sapply(CL2$Models, function(x) summary(x)$coef["x","Estimate"])
## f.se
sapply(CL2$Models, function(x) summary(x)$coef["x","Std. Error"])
## sigma
sapply(CL2$Models, function(x) summary(x)$sigma)


## Forecasting

predict(CL0)
predict(CL1)
predict(CL2)

}
\keyword{ models }
