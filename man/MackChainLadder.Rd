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
MackChainLadder(Triangle, weights = 1/Triangle)

\method{print}{MackChainLadder}(x, \dots)

\method{plot}{MackChainLadder}(x, mfrow=c(3,2), title=NULL, \dots)

\method{summary}{MackChainLadder}(object, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{ a cumulative claims triangle.  A quadratic (nxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i, i=1,\ldots,n }}
  \item{weights}{ weights. Default: 1/Triangle} 
  \item{x, object}{an object of class \code{"MackChainLadder"}}
  \item{mfrow}{see \code{\link{par}}}
  \item{title}{see \code{\link{title}}}  
  \item{\dots}{not in use}
  
}
\details{

Let \eqn{C_{ik}} denote the cumulative loss amounts of origin year \eqn{i=1,\ldots,n}, with losses know for development year \eqn{k \leq n+1-i}.
In order to forecast the amounts \eqn{C_{ik}} for  \eqn{k > n+1-i} the Mack chain-ladder-model assumes:
  \deqn{ E[ \frac{C_{i,k+1}}{C_{ik}} | C_{i1},C_{i2},\ldots,C_{ik} ] = f_k }
  \deqn{ Var( \frac{C_{i,k+1}}{C_{ik}} | C_{i1},C_{i2},\ldots,C_{ik} ) = \frac{\sigma_k^2}{C_{ik}} }
  \deqn{\{ C_{i1},\ldots,C_{in}\}, \{ C_{j1},\ldots,C_{jn}\},\; are\; independent\; for\; origin\; year\; i \neq j }
If these assumptions are hold, the Mack chain-ladder-model gives an unbiased estimator for IBNR (Incurred But Not Reported) claims.
  
The chain-ladder model can be regarded as weighted linear regression through the 
origin for each development year: \code{lm(y ~ x  + 0, weights=1/x)}, 
where \code{y} is the vector of claims at development year k+1 and \code{x} is 
the vector of claims at development year k. 

A tail factor is not yet implemented.
}
\value{

  \item{Triangle}{input triangle of cumulative claims}
  \item{FullTriangle}{forecasted full triangle}
  \item{Models}{linear regression models for each development year}
  \item{f}{chain-ladder ratios}
  \item{f.se}{standard error for chain-ladder ratios}
  \item{sigma}{chain-ladder ratio variance}
  \item{Mack.S.E}{Mack's estimated standard error}
  
}
\references{
Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. Astin Bulletin. Vol. 23. No 2. 1993. pp.213:225
\url{http://www.casact.org/library/astin/vol23no2/213.pdf}

Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. Astin Bulletin. Vol. 29. No 2. 1999. pp.361:366
\url{http://www.casact.org/library/astin/vol29no2/361.pdf}
}
\author{Markus Gesmann \email{markus.gesmann@web.de}}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See also \code{\link{MunichChainLadder}}, \code{\link{residuals.MackChainLadder}} }
\examples{

# Run off triangle of claims data. A data frame with 9 underwriting years and 
# 9 development years.
# Source: Competition Presented at a London Market Actuaries Dinner, D.E.A. Sanders, 
# 1990
Mortgage <- t(matrix(c(
 58046, 127970,  476599, 1027692, 1360489, 1647310, 1819179, 1906852, 1950105,
 24492, 141767,  984288, 2142656, 2961978, 3683940, 4048898, 4115760,      NA,
 32848, 274682, 1522637, 3203427, 4445927, 5158781, 5342585,      NA,      NA,
 21439, 529828, 2900301, 4999019, 6460112, 6853904,      NA,      NA,      NA,
 40397, 763394, 2920745, 4989572, 5648563,      NA,      NA,      NA,      NA,
 90748, 951994, 4210640, 5866482,      NA,      NA,      NA,      NA,      NA,
 62096, 868480, 1954797,      NA,      NA,      NA,      NA,      NA,      NA,
 24983, 284441,      NA,      NA,      NA,      NA,      NA,      NA,      NA,
 13121,     NA,      NA,      NA,      NA,      NA,      NA,      NA,      NA
 ), ncol=9))


MRT <- MackChainLadder(Mortgage)
MRT
plot(MRT) # We observe trends along calendar years.


# Run off triangle of accumulated claims data. 
# A data frame with 10 underwriting years and 10 development years.
# Source: Second moments of estimates of outstanding claims, G.C. Taylor & F.R. Ashe, 
# Journal of Econometrics, 23, pp 37-61
# See Also: Distribution-free Calculation of the Standard Error of Chain Ladder Reserve
# Estimates, Thomas Mack, 1993, ASTIN Bulletin 23, 213 - 225
#
# Peter England and Richard Verrall, Analytic and bootstrap estimates of prediction errors 
# in claims reserving Insurance, Mathematics and Economics Vol. 25, pp 281-293, 1999
#
# P.D.England and R.J.Verrall, Stochastic Claims Reserving in General Insurance, 
# British Actuarial Journal, Vol. 8, pp 443-544, 2002
GenIns <- t(matrix(c(
  357848, 1124788, 1735330, 2218270, 2745596, 3319994, 3466336, 3606286, 3833515, 3901463,
  352118, 1236139, 2170033, 3353322, 3799067, 4120063, 4647867, 4914039, 5339085,      NA,
  290507, 1292306, 2218525, 3235179, 3985995, 4132918, 4628910, 4909315,      NA,      NA,
  310608, 1418858, 2195047, 3757447, 4029929, 4381982, 4588268,      NA,      NA,      NA,
  443160, 1136350, 2128333, 2897821, 3402672, 3873311,      NA,      NA,      NA,      NA,
  396132, 1333217, 2180715, 2985752, 3691712,      NA,      NA,      NA,      NA,      NA,
  440832, 1288463, 2419861, 3483130,      NA,      NA,      NA,      NA,      NA,      NA,
  359480, 1421128, 2864498,      NA,      NA,      NA,      NA,      NA,      NA,      NA,
  376686, 1363294,      NA,      NA,      NA,      NA,      NA,      NA,      NA,      NA,
  344014,      NA,      NA,      NA,      NA,      NA,      NA,      NA,      NA,      NA
  ), ncol=10))

GNI <- MackChainLadder(GenIns)
GNI
plot(GNI)
 
  
# Run off triangle of accumulated claims data. A data frame with 10 underwriting years and 10 
# development years.
# Source:Historical Loss Development, Reinsurance Association of America (RAA), 1991, p.96
# See Also: Which Stochastic Model is Underlying the Chain Ladder Method?, Thomas Mack, 1994,
# Insurance Mathematics and Economics, 15, 2/3, 133-138
#
# P.D.England and R.J.Verrall, Stochastic Claims Reserving in General Insurance, 
# British Actuarial Journal, Vol. 8, pp443-544, 2002
RAA <- t(matrix(c(
  5012,  8269, 10907, 11805, 13539, 16181, 18009, 18608, 18662, 18834,
   106,  4285,  5396, 10666, 13782, 15599, 15496, 16169, 16704,    NA,
  3410,  8992, 13873, 16141, 18735, 22214, 22863, 23466,    NA,    NA,
  5655, 11555, 15766, 21266, 23425, 26083, 27067,    NA,    NA,    NA,
  1092,  9565, 15836, 22169, 25955, 26180,    NA,    NA,    NA,    NA,
  1513,  6445, 11702, 12935, 15852,    NA,    NA,    NA,    NA,    NA,
   557,  4020, 10946, 12314,    NA,    NA,    NA,    NA,    NA,    NA,
  1351,  6947, 13112,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
  3133,  5395,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
  2063,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA
  ), ncol=10))
  
 
 MCL=MackChainLadder(RAA)
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
 MCL2=MCL
 MCL2[["Models"]][[1]] = newModel
 predict(MCL2) # predict the full triangle with the new model 
 #(only the last origin year will be affected)

 MCL2[["FullTriangle"]] <-  predict(MCL2)
 MCL2[["FullTriangle"]] 
 MCL2   # Std. Errors have not been re-estimated!
 # plot the result
 
 plot(MCL2, title="Change MCL Model")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
