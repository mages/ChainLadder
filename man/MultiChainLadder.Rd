\name{MultiChainLadder}
\alias{MultiChainLadder}

\title{ Multivariate Chain Ladder (MCL) Model }
\description{
  The Multivariate Chain Ladder (MCL) model forecasts reserves or IBNR (Incurred But Not Reported)
  claims based on several cumulative claims development triangles simultaneously using a diagonal development matrix and estimates
  the standard errors.
}
\usage{
MultiChainLadder(triangles,fit.method="SUR", delta=1,
	extrap="Mack",mse.method="Mack",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{triangles}{a list of cumulative claims triangles. }
  \item{fit.method}{ method to estimate the development parameters. Default: "SUR", i.e. seemingly unrelated regressions.} 
  \item{delta}{weights. Used to determine the variance structure \eqn{D(Y_{i,k}^{-\delta/2})\Sigma_k D(Y_{i,k}^{-\delta/2})}. It defaults to 1. }   
  \item{extrap}{how to extrapolate the the covariance matrix in the last period.  If \code{extrap="Mack"}, then \code{mcl} uses the Mack extrapolation method. If \code{"Genvar"}, then it uses the generalized variance method. If the data are  trapezoids, then one should use \code{extrap=FALSE}, where no extrapolation is performed.}
  \item{mse.method}{method to estimate the mean square error. Could be either "Mack" or "Murphy", multivariate generalization of the Mack and Murphy formulas respectively.}
  \item{...}{arguments passed to \code{systemfit}}
}
\details{
  This function analyzes the MCL model proposed by Prohl and Schmidt (2005). It has a diaganol development matrix and hence is a special case of the GMCL model. The \code{mcl} function estimates the development matrix using \code{systemfit} and calculates the conditional mean square error using either the multivariate Mack or the multivariaet Murphy&BBMW method.  

  If \code{fit.method="OLS"} is specified, it will fit several separate univariate chain ladders. When one triangle is specified (as a list),  \code{mcl} is equivalent to \code{MackChainLadder}. It also works for trapezoids. It is also equivalent to the \code{gmcl} function with diagonal development matrix restriction under the SUR method. 
}
\value{
  \code{MultiChainLadder} returns a list with the following elements:
  \item{triangles}{input triangles of cumulative claims}
  \item{models}{SUR models for each development period}
  \item{B}{estimated development matrix}
  \item{Bcov}{estimated variance-covariance matrix for \eqn{vec(B_k)}}
  \item{ecov}{\eqn{\Sigma_k}}
  \item{fit.method}{estimation method}
  \item{delta}{value of delta}
  \item{extrap}{extrapolation method}
  \item{mse.ay}{mean square error matrix for each accident year}
  \item{mse.ay.est}{estimation error matrix for each accident year}
  \item{mse.ay.proc}{process error matrix for each accident year}
  \item{mse.total}{mean square error matrix for all accident years combined}
  \item{mse.total.est}{estimation error matrix for all accident years combined}
  \item{mse.total.proc}{process error matrix for all accident years combined}
  \item{fullTriangles}{forecasted full triangles}
  \item{mse.method}{method to estimate mean square error}
}
\references{
\cite{Buchwalder M, Buhlmann H, Merz M, Wuthrich M.V (2006). The mean square error of prediction in the chain ladder reserving method (Mack and Murphy revisited), \emph{ASTIN Bulletin}, 36(2), 521-542.}

\cite{Prohl C, Schmidt K.D (2005). Multivariate chain-ladder, \emph{Dresdner Schriften zur Versicherungsmathematik}.}

\cite{ Mack T (1993). Distribution-free calculation of the standard error, \emph{ASTIN Bulletin}, 23, No.2. }

\cite{ Mack T (1999). The standard error of chain ladder reserve estimates: recursive calculation and inclusion of a tail factor, \emph{ASTIN Bulletin}, 29, No.2, 361-366.}

\cite{ Merz M, Wuthrich M (2008). Prediction error of the multivariate chain ladder reserving method, \emph{North American Actuarial Journal}, 12, No.2, 175-197.}

\cite{ Murphy D. M (1994). Unbiased loss development factors, \emph{Proceedings of the Casualty Actuarial Society}, LXXXI, 154-222.}

\cite{Zhang Y (2009). A general multivariate chain ladder model.}

\cite{Zhang Y (2010). Prediction error of multivariate reserving models in the chain ladder framework.}

}
\author{Yanwei (Wayne) Zhang \email{actuaryzhang@uchicago.edu}}



\examples{

# Reproduce the Mack results. Compare MackChainLadder and MultiChainLadder. 
data(GenIns)
uni1=MackChainLadder(GenIns,est.sigma="Mack") 
uni2=MultiChainLadder(list(GenIns),fit.method="OLS",extrap="Mack")
summary(uni1)
summary(uni2)



# Reproduce the BBMW result using "Murphy" method
fit.bbmw=MultiChainLadder(list(GenIns),fit.method="OLS", delta=1, 
		extrap="Mack", mse.method="Murphy")

fit.bbmw$B
summary(fit.bbmw)



# Compare the mcl and gmcl functions under Multivariate Chain Ladder
 
data(liab)
liab2=extract.cols(liab,1:10)
coefr=matrix(0,4,2)	#impose parameter restriction in gmcl
coefr[1,1]=coefr[4,2]=1
fit1=MultiChainLadder(liab2,extrap = FALSE)
fit2=GeneralMultiChainLadder(liab2,restrict.regMat=coefr)
summary(fit1)	# same results
summary(fit2)



# Reproduce results in Zhang (2010)


### fit separate univariate chain ladders
fit.uni.mack=MultiChainLadder(liab,"OLS",extrap="Mack",mse.method="Mack")
fit.uni.murphy=MultiChainLadder(liab,"OLS",extrap="Mack",mse.method="Murphy")

#summarize results
summary(fit.uni.mack)
summary(fit.uni.murphy)


# fit MultiChainLadder model, but apply scl in later development

liab1=extract.cols(liab,1:10)
liab2=extract.cols(liab,10:14)

#Fit the model and predict triangles

models1 = fit.MultiChainLadder(triangles = liab1,fit.method = "SUR", extrap = FALSE)
models2 = fit.MultiChainLadder(triangles = liab2, fit.method = "OLS", extrap = "Mack")

models=list(triangles=liab,
			models=c(models1$models,models2$models),
			B=c(models1$B,models2$B),
			Bcov=c(models1$Bcov,models2$Bcov),
			ecov=c(models1$ecov,models2$ecov),
			delta=models1$delta)
			
fullTriangles = predict.MultiChainLadderFit(models)			
			
# estimate mse using Mack
mse.mack = mse.MultiChainLadder(mse.method = "Mack",models = models, 
	   		      fullTriangles = fullTriangles)

# estimate mse using Murphy        			
mse.murphy = mse.MultiChainLadder(mse.method = "Murphy", models = models,
	     			 fullTriangles = fullTriangles)
        			
# combine model estimation and mse estimation        
fit.mack=c(models,mse.mack)
fit.murphy=c(models,mse.murphy)

# summarize results
summary.GeneralMultiChainLadder(fit.mack) 
summary.GeneralMultiChainLadder(fit.murphy)



# iterative estimation
for (iter in 1:5){
	fit.iter=MultiChainLadder(liab,fit.method="SUR",delta=1,
		extrap="Mack",mse.method="Murphy",
		control=systemfit.control(methodResidCov="geomean",
			residCovRestricted=TRUE,maxiter=iter))
	print(summary(fit.iter)[[3]][14,4:5])
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
