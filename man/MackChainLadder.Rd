\name{MackChainLadder}
\alias{MackChainLadder}
\alias{print.MackChainLadder}
\alias{summary.MackChainLadder}
\alias{plot.MackChainLadder}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Mack-Chain-Ladder Model }
\description{
  Mack-chain-ladder model to forecast IBNR claims based on a cumulative
  claims triangle.
}
\usage{
MackChainLadder(Triangle, weights = 1/Triangle, tail=FALSE)

\method{print}{MackChainLadder}(x, \dots)

\method{plot}{MackChainLadder}(x, mfrow=c(3,2), title=NULL, \dots)

\method{summary}{MackChainLadder}(object, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{ a cumulative claims triangle.  A quadratic (nxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i, i=1,\ldots,n }, see \code{\link{qpaid}} for how to use (mxn)-development triangles with m<n.}
  \item{weights}{ weights. Default: 1/Triangle} 
  \item{tail}{can be logical or a numeric value. If tail=FALSE no tail factor will be applied, if tail=TRUE a tail factor will be estimate via a linear extrapolation of log(chain ladder factors - 1), if tail is a numeric value than this value will be used as tail factor. }
  \item{x, object}{an object of class \code{"MackChainLadder"}}
  \item{mfrow}{see \code{\link{par}}}
  \item{title}{see \code{\link{title}}}  
  \item{\dots}{not in use}
  
}
\details{

Let \eqn{C_{ik}} denote the cumulative loss amounts of origin period (e.g. accident year) 
\eqn{i=1,\ldots,n}, with losses know for development period \eqn{k \leq n+1-i}.
In order to forecast the amounts \eqn{C_{ik}} for  \eqn{k > n+1-i} the Mack chain-ladder-model assumes:
  \deqn{ E[ \frac{C_{i,k+1}}{C_{ik}} | C_{i1},C_{i2},\ldots,C_{ik} ] = f_k }
  \deqn{ Var( \frac{C_{i,k+1}}{C_{ik}} | C_{i1},C_{i2},\ldots,C_{ik} ) = \frac{\sigma_k^2}{C_{ik}} }
  \deqn{\{ C_{i1},\ldots,C_{in}\}, \{ C_{j1},\ldots,C_{jn}\},\; are\; independent\; for\; origin\; period\; i \neq j }
If these assumptions are hold, the Mack chain-ladder-model gives an unbiased estimator for IBNR 
(Incurred But Not Reported) claims.
  
The chain-ladder model can be regarded as weighted linear regression through the 
origin for each development period: \code{lm(y ~ x  + 0, weights=1/x)}, 
where \code{y} is the vector of claims at development period k+1 and \code{x} is 
the vector of claims at development period k. 

}
\value{
  \item{call}{the matched call} 
  \item{Triangle}{input triangle of cumulative claims}
  \item{FullTriangle}{forecasted full triangle}
  \item{Models}{linear regression models for each development period}
  \item{f}{chain-ladder ratios}
  \item{f.se}{standard error for chain-ladder ratios}
  \item{F.se}{standard error for individual age-to-age ratios}
  \item{sigma}{chain-ladder ratio variance}
  \item{Mack.S.E}{Mack's estimated standard error for the reserves}
  \item{Total.Mack.S.E}{Mack's estimated overall standard error for the reserves}
  \item{tail}{tail factor used. It tail was set to TRUE the output will include the linear model used to estimate the tail factor}  
}
\references{
Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. Astin Bulletin. Vol. 23. No 2. 1993. pp.213:225
\url{http://www.casact.org/library/astin/vol23no2/213.pdf}

Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. Astin Bulletin. Vol. 29. No 2. 1999. pp.361:366
\url{http://www.casact.org/library/astin/vol29no2/361.pdf}
}
\author{Markus Gesmann \email{markus.gesmann@gmail.com}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See also \code{\link{MunichChainLadder}}, \code{\link{qpaid}}, \code{\link{residuals.MackChainLadder}} }
\examples{



Mortgage
MRT <- MackChainLadder(Mortgage)
MRT
plot(MRT) # We observe trends along calendar years.

GenIns
GNI <- MackChainLadder(GenIns)
GNI
plot(GNI)
## include a tail factor
GNIt <- MackChainLadder(GenIns, tail=TRUE)
## extract more information on the tail factor
GNIt$tail 
  
RAA  
MCL <- MackChainLadder(RAA)
MCL
plot(MCL)
 
 
 # investigate in more detail
 MCL[["Models"]][[1]]   # Model for first development period
 summary( MCL[["Models"]][[1]]) # Look at the model stats
 op=par(mfrow=c(2,2)) # plot residuals
   plot( MCL[["Models"]][[1]])
 par(op)

 # let's include an intercept in our model
 newModel <- update(MCL[["Models"]][[1]], y ~ x+1, 
              weights=1/MCL[["Triangle"]][1:9,1],
              data=data.frame(x=MCL[["Triangle"]][1:9,1], 
                              y=MCL[["Triangle"]][1:9,2])
               ) 

# view the new model
 summary(newModel)
 op=par(mfrow=c(2,2)) 
   plot( newModel )
 par(op)

 # change the model for dev. period one to the newModel
 MCL2 <- MCL
 MCL2[["Models"]][[1]] <- newModel
 predict(MCL2) # predict the full triangle with the new model 
 #(only the last origin year will be affected)

 MCL2[["FullTriangle"]] <-  predict(MCL2)
 MCL2[["FullTriangle"]] 
 MCL2   # Std. Errors have not been re-estimated!
 # plot the result
 
 plot(MCL2, title="Changed MCL Model")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
