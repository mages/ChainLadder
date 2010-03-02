\name{MultiChainLadder}
\alias{MultiChainLadder}

\title{ Multivariate Chain Ladder Models }
\description{
  The MultiChainLadder implements multivariate methods within the chain ladder framework to forecass reserves or IBNR (Incurred But Not Reported)
  claims based on several cumulative claims development triangles simultaneously. It fits development models that reflect both contemporaneous correlations and structural relationship, and estimates the conditional Mean Square Errors (MSE). 
}
\usage{
MultiChainLadder(Triangles, 
		fit.method = "SUR", 
		delta = 1, 
		extrap = TRUE, 
		mse.method = "Mack", 
		model = "MCL", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangles}{a list of cumulative claims triangles. }
  \item{fit.method}{ method to estimate the development parameters. Default: "SUR", i.e. seemingly unrelated regressions.} 
  \item{delta}{weights. Used to determine the variance structure \eqn{D(Y_{i,k}^{-\delta/2})\Sigma_k D(Y_{i,k}^{-\delta/2})}. It defaults to 1. }   
  \item{extrap}{logical. Whether to use Mack's extrapolation method for the last period to get the variance component estimation. It only works for \code{model="MCL"}. If the data are  trapezoids, it is set to be \code{FALSE} automatically and a warning message is given.}
  \item{mse.method}{method to estimate the mean square error. Could be either \code{Mack} or \code{Independence}, multivariate generalization of the Mack and Murphy&BBMW formulas respectively.}
  \item{model}{ structure of the model to be fitted. Either \code{MCL} or \code{GMCL}. See details. }
  \item{\dots}{arguments passed to \code{systemfit}.}

}
\details{
This function fits the multivariate models within the chain ladder framework. Corresponding to the \code{model} argument, there are two major models that are incorporated into this function. One is the Multivariate Chain Ladder (MCL) model proposed by Prohl and Schmidt (2005), which is characterized by a diagonal development matrix, allowing multiple lines to be developed together while reflecting the correlations among lines. The other is a natural generalization of the MCL model, the General Multivariate Chain Ladder (GMCL) model proposed by Zhang (2009), which has a non-diagonal development matrix, and can be used to develop structurally related triangles, such as paid and incurred or paid and case reserve, as well as contemporaneously related ones. The MCL model is a sub-model of GMCL, but it is programmed separately because: a) its stand-alone importance; b) different MSE methods are only available for the MCL model; c) extrapolation is not allowed for GMCL. 

Some technical details about the GMCL model. Assume N triangles are available. Denote \eqn{Y_{i,k}=(Y^{(1)}_{i,k}, \ldots ,Y^{(N)}_{i,k})} as an \eqn{N \times 1} vector of  cumulative losses at accident year i and development year k where (n) refers to the n-th triangle. The GMCL model in development period k is:
  \deqn{Y_{i,k+1}=B_k \cdot Y_{i,k}+\epsilon_{i,k}.}
Assumptions for this model are:
\deqn{E(\epsilon_{i,k}|Y_{i,1},\ldots,Y_{i,I+1-k})=0.}
\deqn{cov(\epsilon_{i,k}|Y_{i,1},\ldots,Y_{i,I+1-k})=\Sigma_{\epsilon_{i,k}}=D(Y_{i,k}^{-\delta/2})\Sigma_k D(Y_{i,k}^{-\delta/2}).}
\deqn{\epsilon_{i,k} \, \mbox{of different accident years are independent}.}
\deqn{\epsilon_{i,k} \, \mbox{are symmetrically distributed.}}

The GMCL model is very flexible since different parameter restrictions can be specified. It will be equivalent to the MCL model if the development matrix is restricted to be diagonal. When applied to paid and incurred triangles, it can reflect the development relationship between the two triangles, as described by Quarg and Mack (2004). The full bivariate model is identical to the "double regression" model described by Mack (2003), which is argued by him to be equivalent to the Munich Chain Ladder (MuCL) model. 

Currently the  model \code{GMCL} can only work for trapezoid data, and only allows for estimation method \code{mse.method="Mack"}, while the model \code{MCL} allows extrapolation and the mse method that assumes independence among estimated parameters. The model \code{MCL} under estimation method \code{"OLS"} will be equivalent to separate chain ladders. When one triangle is specified (as a list),  \code{MCL} is equivalent to \code{MackChainLadder}. 

In using the multivariate method, one often specifies separate chain ladder for later periods to stabilize the estimation. In this case, one can use \code{"["}, defined for class \code{triangles} to split the input data, and use the \code{MultiChainLadder} to fit two models, either \code{MCL} or \code{GMCL}, and join them together using \code{Join2Fits}, which creates an object of class \code{MCLFit} or \code{GMCLFit}. Then methods of \code{predict} and \code{Mse} can be called to produce predictions and mean square errors. The function \code{JoinFitMse} is written to make it easy to construct an object of class \code{MultiChainLadder}, for which a couple of methods are defined to produce statistical results and diagonostic plots. 



}

\value{
  \code{MultiChainLadder} returns an object of class \code{MultiChainLadder}  with the following slots:
  \item{model}{model structure used, either \code{MCL} or \code{GMCL}}
  \item{Triangles}{input triangles of cumulative claims, converted to class \code{triangles}}
  \item{models}{fitted models for each development period, output from the call of \code{systemfit}}
  \item{B}{estimated development matrix}
  \item{Bcov}{estimated variance-covariance matrix for \eqn{vec(B_k)}}
  \item{ecov}{\eqn{\Sigma_k}}
  \item{fit.method}{estimation method}
  \item{delta}{value of delta}
  \item{mse.ay}{mean square error matrix for each accident year}
  \item{mse.ay.est}{estimation error matrix for each accident year}
  \item{mse.ay.proc}{process error matrix for each accident year}
  \item{mse.total}{mean square error matrix for all accident years combined}
  \item{mse.total.est}{estimation error matrix for all accident years combined}
  \item{mse.total.proc}{process error matrix for all accident years combined}
  \item{FullTriangles}{forecasted full triangles of class \code{triangles}}
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

\author{ Wayne (Yanwei) Zhang \email{actuaryzhang@uchicago.edu}} 

\seealso{See also \code{\link{MackChainLadder}}, \code{\link{MunichChainLadder}} and \code{\linkS4class{MultiChainLadder}}. }
\examples{

# This shows that MCL under "OLS" applied to one triangle 
# is equivalent to MackChainLadder using the Mack extrapolation

data(GenIns)

uni1 <- MackChainLadder(GenIns,est.sigma="Mack")
uni2 <- MultiChainLadder(list(GenIns),
			fit.method="OLS",
			extrap=TRUE,
			model="MCL")
summary(uni1)
summary(uni2)

\dontrun{
# This compares the plot function from the two different methods
# plot using the univairate function
plot(uni1)
# plot using the multivariate function
X11()
par(mfrow=c(2,2))
plot(uni2)
graphics.off() 
}

# This illustrates the use of the "Independence" assumption in 
# calculating the Mse, which is equivalent to the result in BBMW

fit.bbmw <- MultiChainLadder(list(GenIns),
				fit.method="OLS", 
				delta=1, 
                		extrap=TRUE, 
				mse.method="Independence",
				model="MCL")
print(fit.bbmw)

# MCL and GMCL will be equivalent if appropriate parameter restrictions
# are applied in GMCL and "SUR" method is used
# Notice that GMCL does not work for triangles that need to be extrapolated

data(liab)

liab <- as(liab,"triangles")  # transform "list" to be "triangles"
liab2 <- liab[,1:10]   # special "[" is defined for class "triangles"

#impose parameter restriction in GMCL so that development matrix is diagonal
coefr <- matrix(0,4,2)     
coefr[1,1] <- coefr[4,2] <- 1

fit1 <- MultiChainLadder(liab2,extrap = FALSE,model="MCL")
fit2 <- MultiChainLadder(liab2,restrict.regMat=coefr,model="GMCL")

summary(fit1)
summary(fit2)


# Reproduce results in Zhang(2010)
# The idea is to apply MCL so that correlation will be reflected. 
# We apply MCL to the first 10 development periods, and SCL to the rest  

### fit SCL for the two triangles under different methods of mse estimation

fit.uni.mack <- MultiChainLadder(liab,"OLS",mse.method="Mack")
fit.uni.indep <- MultiChainLadder(liab,"OLS",mse.method="Independence")

#summarize results
summary(fit.uni.mack)
summary(fit.uni.indep)

# partition the data into two sets

liab1 <- liab[,1:10]
liab2 <- liab[,10:14]

#Fit the model and predict triangles
models1 <- MultiChainLadder(Triangles = liab1,
				fit.method = "SUR", 
				extrap = FALSE)

models2 <- MultiChainLadder(Triangles = liab2, 
				fit.method = "OLS")

# Join the two fits together and create an object of class "MCLFit"
models <- Join2Fits(models1,models2)
class(models)

# Complete the triangles                        
FullTriangles  <-  predict(models)                       

# estimate mse using Mack
mse.mack  <-  Mse(ModelFit = models, 
                   FullTriangles = FullTriangles,
		   mse.method = "Mack")

# estimate mse using Murphy&BBMW                             

mse.murphy  <-  Mse( ModelFit = models,
                      FullTriangles = FullTriangles,
		      mse.method = "Independence")
                               
# combine model estimation and mse estimation, and create an object of class "MultiChainLadder"        
fit.mack <- JoinFitMse(models,mse.mack)
class(fit.mack)
fit.murphy <- JoinFitMse(models,mse.murphy)

# summarize results, portfolio statistics are automatically calculated
summary(fit.mack) 
summary(fit.murphy)

\dontrun{
# generate diagonostic plot
X11()
par(mfrow=c(2,2))
plot(fit.mack,which.plot=1:2,which.triangle=1:2)
X11()
par(mfrow=c(2,2))
plot(fit.mack,which.plot=3:4,which.triangle=1:2)
graphics.off() ; 
}

# Illustrate the Merz and Wuthrich estimator. The unbiased estimator can be specified as
# "Theil" in the systemfit package. This estimator may not be positive semi-definite and 
# is not recommended. 

fit.mw=MultiChainLadder(liab1,mse.method="Independence",
                control=systemfit.control(methodResidCov="Theil"))

summary(fit.mw)

# Iterative estimation can also be used to improve the estimation of the MCL. 
# This can be specified using the "maxiter" option in systemfit. 
# We use the above liab example, and see the result converges in the third step. 

for (iter in 1:5){
	models1 <- MultiChainLadder(Triangles = liab1, 
					extrap = FALSE, 
					control=systemfit.control(maxiter=iter))
        models2 <- MultiChainLadder(Triangles = liab2, 
					fit.method = "OLS",
					control=systemfit.control(maxiter=iter))
        models <- Join2Fits(models1,models2)
        FullTriangles  <-  predict(models)               
        mse.mack  <-  Mse(ModelFit = models, 
               		FullTriangles = FullTriangles,
		   	mse.method = "Mack")
	fit.iter <- JoinFitMse(models,mse.mack)
        print(summary(fit.iter)[[3]][14,4:5])
        }

# Reproduce results in Zhang(2009)
# Data auto consists of three triangles, paid personal auto, incurred personal auto
# and paid commercial auto. The paid and incurred triangles from personal auto will
# result in divergent Paid-to-incurred ratios under SCL. The idea here is to use GMCL
# to all three triangles so that not only contemporaneous correlations can be reflected,
# but the structural relationship between paid and incurred can also be picked up.  
# We will apply GMCL in the first 7 periods, and SCL in the later periods.  

data(auto)
auto <- as(auto,"triangles")

# SCL will result in divegent paid-to-incurred ratios
fit.scl=MultiChainLadder(auto,fit.method="OLS",model="MCL")
ratios1=fit.scl$FullTriangles[[1]][,10]/fit.scl$FullTriangles[[2]][,10]
print(round(ratios1*100,2))

# Munich Chain Ladder applied to the paid and incurred personal auto triangles
fit.mucl=MunichChainLadder(auto[[1]],auto[[2]])
ratios2=summary(fit.mucl)[[1]][,6]
print(round(ratios2*100,2))

# GMCL model
da1 <- auto[,1:7]
da2 <- auto[,7:10]	

# Specify coefficient restrictions. Only allow the paid to depend on the incurred. 
coefr <- matrix(0,9,4)
coefr[1,1] <- coefr[2,2] <- coefr[5,3] <- coefr[9,4] <- 1

# fit model and estimate parameters
fit.restr1 <- MultiChainLadder(da1,restrict.regMat=coefr,model="GMCL")
fit.restr2 <- MultiChainLadder(da2,fit.method="OLS",model="MCL")

# combine the two models together, result is an object of class "GMCLFit"
models <- Join2Fits(fit.restr1,fit.restr2)
class(models)

# predict the full triangles
FullTriangles <- predict(models)

#calculate mse
mse.models <- Mse(ModelFit=models,FullTriangles=FullTriangles)
fit.restr=JoinFitMse(models,mse.models)

# paid-to-incurred ratios close to 1
ratios3=fit.restr$FullTriangles[[1]][,10]/fit.restr$FullTriangles[[2]][,10]
print(round(ratios3*100,2))

# get summary statistics, "1+3" specifies paid+paid as the portfolio
summary(fit.restr,portfolio="1+3")[[4]]   

# compare with SCL that has much smaller mse
summary(fit.scl,portfolio="1+3")[[4]]

\dontrun{
# generate diagonostic plots
X11()
par(mfcol=c(3,2) )
plot(fit.restr,which.plot=3:4,portfolio="1+3",lowess=FALSE)
graphics.off()
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

