\name{GeneralMultiChainLadder}
\alias{GeneralMultiChainLadder}

\title{ General Multivariate Chain Ladder (GMCL) Model }
\description{
  The General Multivariate Chain Ladder (GMCL) model forecasts reserves or IBNR (Incurred But Not Reported)
  claims based on several cumulative claims development triangles simultaneously and estimates
  the standard errors.
}
\usage{
GeneralMultiChainLadder(triangles,fit.method="SUR", delta=1,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{triangles}{a list of cumulative claims triangles.} 
  \item{fit.method}{ method to estimate the development parameters. Default: "SUR", i.e. seemingly unrelated regressions.} 
  \item{delta}{weights. Used to determine the variance structure \eqn{D(Y_{i,k}^{-\delta/2})\Sigma_k D(Y_{i,k}^{-\delta/2})}. It defaults to 1. }   
  \item{...}{arguments passed to \code{systemfit}}
}
\details{
  This function fits the GMCL model described by Zhang (2009). Assume N triangles are available. Denote \eqn{Y_{i,k}=(Y^{(1)}_{i,k}, \ldots ,Y^{(N)}_{i,k})} as an \eqn{N \times 1} vector of  cumulative losses at accident year i and development year k where (n) refers to the n-th triangle. The GMCL model in development period k is:
  \deqn{Y_{i,k+1}=B_k \cdot Y_{i,k}+\epsilon_{i,k}}.
Assumptions for this model are:
\deqn{E(\epsilon_{i,k}|Y_{i,1},\ldots,Y_{i,I+1-k})=0.}
\deqn{cov(\epsilon_{i,k}|Y_{i,1},\ldots,Y_{i,I+1-k})=\Sigma_{\epsilon_{i,k}}D(Y_{i,k}^{-\delta/2})\Sigma_k D(Y_{i,k}^{-\delta/2}).}
\deqn{\epsilon_{i,k} \, \mbox{of different accident years are independent}.}
\deqn{\epsilon_{i,k} \, \mbox{are symmetrically distributed.}}

The \code{GeneralMultiChainLadder} can only work for trapezoid data where there are enough observations at tail development periods. To model triangle data, one needs to apply \code{gmcl} to earlier development periods and separate chain ladder or multivariate chain ladder for later periods. For splitting data, use \code{extract.cols}. 

}
\value{
  \code{GeneralMultiChainLadder} returns a list with the following elements
  \item{triangles}{input triangles of cumulative claims}  
  \item{models}{SUR models for each development period}
  \item{B}{estimated development matrix}
  \item{Bcov}{estimated variance-covariance matrix for \eqn{vec(B_k)}}
  \item{ecov}{\eqn{\Sigma_k}}
  \item{fit.method}{regression method}
  \item{delta}{weight in the regression}
  \item{mse.ay}{mse for each accident year}
  \item{mse.ay.est}{estimation mse for each accident year}
  \item{mse.ay.proc}{process mse for each accident year}
  \item{mse.total}{mse for all accident years combined}
  \item{mse.total.est}{estimation mse for all accident years combined}
  \item{mse.total.proc}{process mse for all accident years combined}
  \item{fullTriangles}{forecasted full triangles}
}
\references{
\cite{Zhang Y (2009). A general multivariate chain ladder model.}

\cite{Zhang Y (2010). Prediction error of multivariate reserving models in the chain ladder framework.}

}
\author{Yanwei (Wayne) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See also \link{fit.GeneralMultiChainLadder}, \link{mse.GeneralMultiChainLadder}
}

\examples{

# Reproduce the results in Zhang's paper

data(auto)
# first 7 columns, to be fitted using GMCL
da1=extract.cols(auto,1:7)
# last 4 columns, to be fitted using separate chain ladder	
da2=extract.cols(auto,7:10)	

# specify coefficient restrictions 
coefr=matrix(0,9,4)
coefr[1,1]=coefr[2,2]=coefr[5,3]=coefr[9,4]=1

# fit model and estimate parameters
fit.restr1=fit.GeneralMultiChainLadder(da1,restrict.regMat=coefr)
fit.restr2=fit.MultiChainLadder(da2,fit.method="OLS",extrap="Mack")

# combine the two models together
B=c(fit.restr1$B,lapply(1:3, function(x) diag(fit.restr2$B[[x]])))
ecov=c(fit.restr1$ecov,lapply(1:3, function(x) fit.restr2$ecov[[x]]))
Bcov=c(fit.restr1$Bcov,lapply(1:3, 
		function(x) transf.Bcov(fit.restr2$Bcov[[x]])))
models=list(triangles=auto,B=B,Bcov=Bcov,ecov=ecov,delta=1)

# predict the full triangles
fullTriangles=predict.GeneralMultiChainLadderFit(models)

#calculate mse
mse=mse.GeneralMultiChainLadder(models=models,fullTriangles=fullTriangles)

fit.restr=c(models,mse)

# get summary statistics, "1+3" specifies paid+paid as the portfolio
summary.GeneralMultiChainLadder(fit.restr,agg="1+3")   


# fit univariate models
fit.uni=MultiChainLadder(auto,fit.method="OLS",extrap="Mack")

#paid + paid
summary.GeneralMultiChainLadder(fit.uni,agg="1+3") 
#incur +paid. 
#need to adjust the IBNR (subtract the latest paid instead) 
summary.GeneralMultiChainLadder(fit.uni,agg="2+3") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
