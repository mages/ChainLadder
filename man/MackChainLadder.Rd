\name{MackChainLadder}
\alias{MackChainLadder}

\title{ Mack-Chain-Ladder Model }
\description{
  The Mack-chain-ladder model forecasts reserves or IBNR (Incurred But Not Reported)
  claims based on a cumulative claims development triangle and estimates
  the standard error around it.
}
\usage{
MackChainLadder(Triangle, weights = 1/Triangle, est.sigma="log-linear",
tail=FALSE, tail.se=NULL, tail.sigma=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{cumulative claims triangle.  A (mxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i; i=1,\ldots,m; m\geq n }, see
    \code{\link{qpaid}} for how to use (mxn)-development triangles with
    m<n, say higher development period frequency (e.g quarterly) than
    origin period frequency (e.g accident years).}
  \item{weights}{ weights. Default: 1/Triangle, to use volume weighted
    chain ladder factors.} 
  \item{est.sigma}{defines how to estimate \eqn{sigma_{n-1}}, the
    variability of the individual age-to-age factors at development time
    \eqn{n-1}.
    Default is "log-linear" for a log-linear regression, "Mack" for
    Mack's approximation from his 1999 paper. Alternatively the user can
    provide a numeric value.}  
  \item{tail}{can be logical or a numeric value.
    If \code{tail=FALSE} no tail factor will be applied, if \code{tail=TRUE} 
    a tail factor will be estimated via a linear extrapolation of
    \eqn{log(chain ladder factors - 1)}, if \code{tail} is a numeric
    value than this value will be used as tail factor.}
  \item{tail.se}{defines how to estimate the standard error of the tail factor. Only needed if
    a tail factor > 1 is provided. Default is \code{NULL}. If \code{tail.se} is \code{NULL},
    \code{tail.se} is estimated via "log-linear" regression, if \code{tail.se} is a numeric
    value than this value will be used instead.}  
  \item{tail.sigma}{defines how to estimate individual tail variability. Only needed if
      a tail factor > 1 is provided.
    Default is \code{NULL}. If \code{tail.sigma} is \code{NULL},
    \code{tail.sigma} 
    is estimated via "log-linear" regression, if \code{tail.sigma} is a numeric
    value than this value will be used instead}  
}
\details{
  Following Mack's 1999 paper let \eqn{C_{ik}} denote the cumulative loss amounts of origin period
  (e.g. accident year) 
  \eqn{i=1,\ldots,m}, with losses known for development period
  (e.g. development year) \eqn{k \le n+1-i}.
  In order to forecast the amounts \eqn{C_{ik}} for
  \eqn{k > n+1-i} the Mack chain-ladder-model assumes:
  \deqn{\mbox{CL1: }  E[ F_{ik}| C_{i1},C_{i2},\ldots,C_{ik} ] = f_k
    \mbox{ with } F_{ik}=\frac{C_{i,k+1}}{C_{ik}}
  }{CL1: E[ F_ik| C_i1,C_i2,...,C_ik ] = f_k with F_ik=C_{i,k+1}/C_ik}
  
  \deqn{\mbox{CL2: }  Var( \frac{C_{i,k+1}}{C_{ik}} | C_{i1},C_{i2},
    \ldots,C_{ik} ) = \frac{\sigma_k^2}{w_{ik} C^\alpha_{ik}}
  }{CL2: Var( C_{i,k+1}/C_ik | C_i1, C_i2, ... ,C_ik ) = sigma_k^2/( w_ik C^alpha_ik)
  }
  \deqn{\mbox{CL3: }  \{ C_{i1},\ldots,C_{in}\}, \{
    C_{j1},\ldots,C_{jn}\},\mbox{ are independent for origin period } i
    \neq j 
  }{CL3: \{ C_i1, ... ,C_in\}, \{
    C_j1, ... ,C_jn\}, are independent for origin period i != j 
  }
  with \eqn{w_{ik} \in [0;1]}{w_{ik} in [0;1]}, \eqn{\alpha \in \{0,1,2\}}{alpha in \{0,1,2\}}.   
  If these assumptions are hold, the Mack-chain-ladder-model gives an
  unbiased estimator for IBNR (Incurred But Not Reported) claims.
  
  The Mack-chain-ladder model can be regarded as a weighted linear regression
  through the origin for each development period:
  \code{lm(y ~ x  + 0, weights=1/x)}, 
  where \code{y} is the vector of claims at development period
  \eqn{k+1} and \code{x} is  the vector of claims at development period
  \eqn{k}. 

}
\value{
  MackChainLadder returns a list with the following elements
  \item{call}{matched call} 
  \item{Triangle}{input triangle of cumulative claims}
  \item{FullTriangle}{forecasted full triangle}
  \item{Models}{linear regression models for each development period}
  \item{f}{chain-ladder-link-ratios}
  \item{f.se}{standard errors of the chain-ladder-link-ratios f
    (assumption CL1)}
  \item{F.se}{standard errors of the true chain-ladder-link-ratios
    \eqn{F_{ik}} (square root of the variance in assumption CL2)}
  \item{sigma}{sigma parameter in CL2}
  \item{Mack.ProcessRisk}{variability in the projection of future losses
    not explained by the variability of the link ratio estimators (unexplained variation)}
  \item{Mack.ParameterRisk}{variability in the projection of future
    losses explained by the variability of the link-ratio estimators
    alone (explained variation)}
  \item{Mack.S.E}{total variability in the projection of future losses
    by the chain ladder method; the square root of the mean square error
    of the chain ladder estimate: \eqn{\mbox{Mack.S.E.}^2 = \mbox{Mack.ProcessRisk}^2 +
    \mbox{Mack.ParameterRisk}^2}{Mack.S.E.^2 = Mack.ProcessRisk^2 +
  Mack.ParameterRisk^2}}
  \item{Total.Mack.S.E}{total variability of projected loss for all origin years combined}
  \item{tail}{tail factor used. If tail was set to TRUE the output will
    include the linear model used to estimate the tail factor}
}
\references{
\cite{Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. \emph{Astin Bulletin}. Vol. 23. No 2. 1993. pp.213:225}

\cite{Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. \emph{Astin Bulletin}. Vol. 29. No 2. 1999. pp.361:366}
}
\author{Markus Gesmann \email{markus.gesmann@gmail.com}}
\note{
  Additional references for further reading:
  
  \cite{England, PD and Verrall, RJ. Stochastic Claims Reserving in General Insurance (with discussion), \emph{British Actuarial Journal} 8, III. 2002}

  \cite{Murphy, Daniel M. Unbiased Loss Development Factors. \emph{Proceedings of the Casualty Actuarial Society Casualty Actuarial Society} - Arlington, Virginia 1994: LXXXI 154-222.}
  
  \cite{Zehnwirth and Barnett. Best estimates for reserves. Proceedings of the CAS, LXXXVI I(167), November 2000.}
  
% ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ See also \code{\link{qpaid}}, 
  \code{\link{summary.MackChainLadder}}, 
  \code{\link{plot.MackChainLadder}}, 
  \code{\link{residuals.MackChainLadder}}, 
  \code{\link{MunichChainLadder}},
  \code{\link{BootChainLadder}},
}
% sqrt(5608^2*(0.03^2+0.02^2)+140^2*1.05^2)
% sqrt(MRT$FullTriangle[3,9]^2 * (MRT$F.se[3,9]^2 + MRT$f.se[9]^2) + MRT$Mack.S.E[3,9]^2*MRT$f[9]^2)

\examples{

# See the Taylor/Ashe example in Mack's 1993 paper
GenIns
GNI <- MackChainLadder(GenIns, est.sigma="Mack")
GNI$f
GNI$sigma^2
GNI # compare to table 2 and 3 in Mack's 1993 paper
plot(GNI)

# See the example in Mack's 1999 paper
Mortgage
MRT <- MackChainLadder(Mortgage, tail=1.05, tail.sigma=71, tail.se=0.02, est.sigma="Mack")
MRT
# Table 1 in the above paper
MRT$f
MRT$f.se
MRT$F.se[3,]
MRT$sigma
plot(MRT) # We observe trends along calendar years.

# Table 2 in the above paper
MRT$FullTriangle[,9]/1000 ## C_{i9}
MRT$FullTriangle[,10]/1000 ## C_{i,ult}
MRT$Mack.S.E[,9]/1000 ## s.e.(C_{i9}), difference due to rounding

# Access process risk error
MRT$Mack.ProcessRisk

# Access parameter risk error
MRT$Mack.ParameterRisk

# Total risk
MRT$Mack.S.E

op <- par(mfrow=c(2,1))
plot(with(summary(MRT)$ByOrigin, Mack.S.E/Ultimate),t="l",
ylab="CV(Ultimate)", xlab="origin period")
plot(summary(MRT)$ByOrigin[["CV(IBNR)"]], t="l", ylab="CV(IBNR)",
xlab="origin period")
par(op)

  
# This data set is discussed many papers, e.g. England and Verall 2000
# paper, see Table 1 just there  
RAA
R <- MackChainLadder(RAA)
R
plot(R)
# Table 12 in England and Verall 2000 paper
R$f
R$sigma^2
# Table 13 in England and Verall 2000 paper
# Please note the different indexing of sigma
MackChainLadder(RAA, est.sigma=R$sigma[7])
# Table 14 in England and Verall 2000 paper
MackChainLadder(RAA, est.sigma=R$sigma[8])

# Use lattice to plot the individual origin years
longRAA <- expand.grid(origin=as.numeric(dimnames(RAA)$origin), dev=as.numeric(dimnames(RAA)$dev))
longRAA$value <- as.vector(R$FullTriangle)
longRAA$valuePlusMack.S.E <-  longRAA$value + as.vector(R$Mack.S.E)
longRAA$valueMinusMack.S.E <- longRAA$value - as.vector(R$Mack.S.E)
library(lattice)
xyplot(valuePlusMack.S.E + valueMinusMack.S.E + value ~ dev |
factor(origin), data=longRAA, t="l", col=c("red","red","black"), as.table=TRUE)


# Different weights
# Using 1/Triangle^2 as weight will use mean chain ladder ratios 
MackChainLadder(RAA, weights=1/RAA^2)$f
apply(RAA[,-1]/RAA[,-10],2, mean, na.rm=TRUE)
 
# Let's investigate the Mack model more detail
R[["Models"]][[1]]   # Model for first development period
summary( R[["Models"]][[1]]) # Look at the model stats
op <- par(mfrow=c(2,2)) # plot residuals
  plot( R[["Models"]][[1]])
par(op)

# Let's include an intercept in our model
newModel <- update(R[["Models"]][[1]], y ~ x+1, 
             weights=1/R[["Triangle"]][1:9,1],
             data=data.frame(x=R[["Triangle"]][1:9,1], 
                             y=R[["Triangle"]][1:9,2])
              ) 

# View the new model
summary(newModel)
op <- par(mfrow=c(2,2)) 
  plot( newModel )
par(op)

# Change the model for dev. period one to the newModel
R2 <- R
R2[["Models"]][[1]] <- newModel
predict(R2) # predict the full triangle with the new model 
#(only the last origin year will be affected)

R2[["FullTriangle"]] <-  predict(R2)
R2[["FullTriangle"]] 
R2   # Std. Errors have not been re-estimated!
# Plot the result
 
plot(R2, title="Changed R Model")

## 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
