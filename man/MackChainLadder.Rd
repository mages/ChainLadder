\name{MackChainLadder}
\alias{MackChainLadder}

\title{ Mack-Chain-Ladder Model }
\description{
  The Mack-chain-ladder model forecasts future claims developments
  based on a historical cumulative claims development triangle and estimates
  the standard error around those.
}
\usage{
MackChainLadder(Triangle, weights = 1, alpha=1, est.sigma="log-linear",
tail=FALSE, tail.se=NULL, tail.sigma=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{cumulative claims triangle.  Assume columns are the development
    period, use transpose otherwise.
    A (mxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i; i=1,\ldots,m; m\geq n }, see
    \code{\link{qpaid}} for how to use (mxn)-development triangles with
    m<n, say higher development period frequency (e.g quarterly) than
    origin period frequency (e.g accident years).}
  \item{weights}{ weights. Default: 1, which sets the weights for all
    triangle entries to 1. Otherwise specify weights as a matrix of the same
    dimension as \code{Triangle} with all weight entries in [0; 1]}
  \item{alpha}{'weighting' parameter.
    Default: 1 for all development periods; alpha=1 gives the historical chain
    ladder age-to-age factors, alpha=0 gives the straight average of the
    observed individual development factors and alpha=2 is the result of
    an ordinary regression of \eqn{C_{i,k+1}} against \eqn{C_{i,k}} with
    intercept 0, see also Mack's 1999 paper and \code{chainladder}}
  \item{est.sigma}{defines how to estimate \eqn{sigma_{n-1}}, the
    variability of the individual age-to-age factors at development time
    \eqn{n-1}.
    Default is "log-linear" for a log-linear regression, "Mack" for
    Mack's approximation from his 1999 paper. Alternatively the user can
    provide a numeric value. If the log-linear model appears to be
    inappropriate (p-value > 0.05) the 'Mack' method will be used
    instead and a warning message printed.
    Similarly, if Triangle is so small that log-linear regression is being
    attempted on a vector of only one non-NA average link ratio,
    the 'Mack' method will be used
    instead and a warning message printed.
    }  
  \item{tail}{can be logical or a numeric value.
    If \code{tail=FALSE} no tail factor will be applied, if \code{tail=TRUE} 
    a tail factor will be estimated via a linear extrapolation of
    \eqn{log(chain ladder factors - 1)}, if \code{tail} is a numeric
    value than this value will be used instead.}
  \item{tail.se}{defines how the standard error of the tail factor is estimated. Only needed if
    a tail factor > 1 is provided. Default is \code{NULL}. If
    \code{tail.se} is \code{NULL}, \code{tail.se} is estimated via
    "log-linear" regression, if \code{tail.se} is a numeric
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
  \code{lm(y ~ x  + 0, weights=w/x^(2-alpha))}, 
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
  \item{f}{chain-ladder age-to-age factors}
  \item{f.se}{standard errors of the chain-ladder age-to-age factors f
    (assumption CL1)}
  \item{F.se}{standard errors of the true chain-ladder age-to-age factors
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
  \item{Total.Mack.S.E}{total variability of projected loss for all
    origin years combined}
  \item{weights}{weights used.}
  \item{alpha}{alphas used.}
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
  
  \cite{England, PD and Verrall, RJ. Stochastic Claims Reserving in
  General Insurance (with discussion), \emph{British Actuarial Journal}
  8, III. 2002}

  \cite{Murphy, Daniel M. Unbiased Loss Development
  Factors. \emph{Proceedings of the Casualty Actuarial Society Casualty
    Actuarial Society} - Arlington, Virginia 1994: LXXXI 154-222.}
  
  \cite{Barnett and Zehnwirth. Best estimates for reserves. Proceedings
    of the CAS, LXXXVI I(167), November 2000.}  
  
% ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ See also \code{\link{qpaid}},  
  \code{\link{chainladder}},  
  \code{\link{summary.MackChainLadder}},  
  \code{\link{plot.MackChainLadder}},  
  \code{\link{residuals.MackChainLadder}}, 
  \code{\link{MunichChainLadder}},
  \code{\link{BootChainLadder}},
}

\examples{

## See the Taylor/Ashe example in Mack's 1993 paper
GenIns
plot(GenIns)
plot(GenIns, lattice=TRUE)
GNI <- MackChainLadder(GenIns, est.sigma="Mack")
GNI$f
GNI$sigma^2
GNI # compare to table 2 and 3 in Mack's 1993 paper
plot(GNI)
plot(GNI, lattice=TRUE)

## Different weights
## Using alpha=0 will use straight average age-to-age factors 
MackChainLadder(GenIns, alpha=0)$f
# You get the same result via:
apply(GenIns[,-1]/GenIns[,-10],2, mean, na.rm=TRUE)

## See the example in Mack's 1999 paper
Mortgage
plot(Mortgage)
MRT <- MackChainLadder(Mortgage, tail=1.05, tail.sigma=71, tail.se=0.02, est.sigma="Mack")
MRT
plot(MRT, lattice=TRUE)

## For more examples see:
\dontrun{
 demo(MackChainLadder)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
