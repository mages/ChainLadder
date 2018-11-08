\name{chainladder}
\Rdversion{1.1}
\alias{chainladder}
\title{ Estimate age-to-age factors }
\description{
Basic chain-ladder function to estimate age-to-age factors for a given
cumulative run-off triangle. This function is used by Mack- and MunichChainLadder.
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
    origin period frequency (e.g annual).
    }
  \item{weights}{weights. Default: 1, which sets the weights for all
    triangle entries to 1. Otherwise specify weights as a matrix of the same
    dimension as \code{Triangle} with all weight entries in [0; 1], where entry
    \eqn{w_{i,k}} corresponds to the point \eqn{C_{i,k+1}/C_{i,k}}. 
    Hence, any entry set to 0 or \code{NA} eliminates that age-to-age factor 
    from inclusion in the model. See also 'Details'.}
  \item{delta}{'weighting' parameters. Default: 1;
    delta=1 gives the historical chain-ladder age-to-age factors, 
    delta=2 gives the straight average of the
    observed individual development factors and delta=0 is the result of
    an ordinary regression of \eqn{C_{i,k+1}} against \eqn{C_{i,k}} with
    intercept 0, see Barnett & Zehnwirth (2000).
    
    Please note that \code{\link{MackChainLadder}} uses the argument \code{alpha}, 
    with \code{alpha = 2 - delta}, following the original paper Mack (1999)
  }
}

\details{
 
  The key idea is to see the chain-ladder algorithm as a special form of a
  weighted linear regression through the origin, applied to each development 
  period.

  Suppose \code{y} is the vector of cumulative claims at development period
  \code{i+1}, and \code{x} at development period \code{i}, \code{weights} are
  weighting factors and \code{F} the individual age-to-age factors \code{F=y/x}. Then
  we get the various age-to-age factors:
\itemize{
  \item{Basic (unweighted) linear regression through the origin:
    \code{lm(y~x + 0)} 
  }
  \item{Basic weighted linear regression through the origin:
    \code{lm(y~x + 0, weights=weights)} 
  }
 \item{Volume weighted chain-ladder age-to-age factors: 
  \code{lm(y~x + 0, weights=1/x)}
  }
 \item{Simple average of age-to-age factors:
  \code{lm(y~x + 0, weights=1/x^2)}
 } 
}
Barnett & Zehnwirth (2000) use delta = 0, 1, 2 to distinguish between the above 
three different regression approaches: \code{lm(y~x + 0, weights=weights/x^delta)}.

Thomas Mack uses the notation \code{alpha = 2 - delta} to achieve the same result:
\code{sum(weights*x^alpha*F)/sum(weights*x^alpha) # Mack (1999) notation}
}
\value{
  chainladder returns a list with the following elements:
  \item{Models}{linear regression models for each development period}
  \item{Triangle}{input triangle of cumulative claims}
  \item{weights}{weights used}
  \item{delta}{deltas used}
}
\references{
  
\cite{Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. \emph{Astin Bulletin}. Vol. 29. No 2. 1999. pp.361:366}

\cite{G. Barnett and B. Zehnwirth. Best Estimates for
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
\code{\link{ata}},	
 \code{\link{predict.ChainLadder}}
 \code{\link{MackChainLadder}},  
}
\examples{
## Concept of different chain-ladder age-to-age factors.
## Compare Mack's and Barnett & Zehnwirth's papers.
x <- RAA[1:9,1]
y <- RAA[1:9,2]

F <- y/x
## wtd. average chain-ladder age-to-age factors
alpha <- 1 ## Mack notation
delta <- 2 - alpha ## Barnett & Zehnwirth notation

sum(x^alpha*F)/sum(x^alpha)
lm(y~x + 0 ,weights=1/x^delta)
summary(chainladder(RAA, delta=delta)$Models[[1]])$coef

## straight average age-to-age factors
alpha <- 0
delta <- 2 - alpha 
sum(x^alpha*F)/sum(x^alpha)
lm(y~x + 0, weights=1/x^(2-alpha))
summary(chainladder(RAA, delta=delta)$Models[[1]])$coef

## ordinary regression age-to-age factors
alpha=2
delta <- 2-alpha
sum(x^alpha*F)/sum(x^alpha)
lm(y~x + 0, weights=1/x^delta)
summary(chainladder(RAA, delta=delta)$Models[[1]])$coef

## Compare different models
CL0 <- chainladder(RAA)
## age-to-age factors
sapply(CL0$Models, function(x) summary(x)$coef["x","Estimate"])
## f.se
sapply(CL0$Models, function(x) summary(x)$coef["x","Std. Error"])
## sigma
sapply(CL0$Models, function(x) summary(x)$sigma)
predict(CL0)

CL1 <- chainladder(RAA, delta=1)
## age-to-age factors
sapply(CL1$Models, function(x) summary(x)$coef["x","Estimate"])
## f.se
sapply(CL1$Models, function(x) summary(x)$coef["x","Std. Error"])
## sigma
sapply(CL1$Models, function(x) summary(x)$sigma)
predict(CL1)

CL2 <- chainladder(RAA, delta=2)
## age-to-age factors
sapply(CL2$Models, function(x) summary(x)$coef["x","Estimate"])
## f.se
sapply(CL2$Models, function(x) summary(x)$coef["x","Std. Error"])
## sigma
sapply(CL2$Models, function(x) summary(x)$sigma)
predict(CL2)

## Set 'weights' parameter to use only the last 5 diagonals, 
## i.e. the last 5 calendar years
(weights <- ifelse((row(RAA) + col(RAA) - 1) <= 5, 0, 
              ifelse(row(RAA) + col(RAA) - 1 > 10, NA, 1)))
CL3 <- chainladder(RAA, weights=weights)
summary(CL3$Models[[1]])$coef
predict(CL3)
}
\keyword{ models }
