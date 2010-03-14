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
		int = NULL,
		restrict.regMat = NULL,
		extrap = TRUE, 
		mse.method = "Mack", 
		model = "MCL", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangles}{a list of cumulative claims triangles. }
  \item{fit.method}{ method to estimate the development parameters. Default: "SUR", i.e. seemingly unrelated regressions.} 
  \item{delta}{parameter for weights. Used to determine the covariance structure \eqn{D(Y_{i,k}^{-\delta/2})\Sigma_k D(Y_{i,k}^{-\delta/2})}. It defaults to 1. }   
  \item{int}{indicator of which periods have intercepts. This only works for \code{GMCL}. Default \code{NULL} means no intercept. Otherwise, specify a numeric vector. }   
  \item{restrict.regMat}{a list of matrix specifying parameter restriction matrix for each period. This only works for \code{GMCL}. Default \code{NULL} means no restriction. See \code{systemfit} } 
  \item{extrap}{logical. Whether to use Mack's extrapolation method for the last period to get the variance component estimation. It only works for \code{model="MCL"}. If the data are  trapezoids, it is set to be \code{FALSE} automatically and a warning message is given.}
  \item{mse.method}{method to estimate the mean square error. Could be either \code{Mack} or \code{Independence}, multivariate generalization of the Mack and Murphy&BBMW formulas respectively.}
  \item{model}{ structure of the model to be fitted. Either \code{MCL} or \code{GMCL}. See details. }
  \item{\dots}{arguments passed to \code{systemfit}.}

}
\details{
This function fits the multivariate models within the chain ladder framework. Corresponding to the \code{model} argument, there are two major models that are incorporated into this function. One is the Multivariate Chain Ladder (MCL) model proposed by Prohl and Schmidt (2005), which is characterized by a diagonal development matrix, allowing multiple lines to be developed together while reflecting the correlations among lines. The other is a natural generalization of the MCL model, the General Multivariate Chain Ladder (GMCL) model proposed by Zhang (2009), which has a non-diagonal development matrix and  intercepts, and can be used to develop structurally related triangles, such as paid and incurred or paid and case reserve, as well as contemporaneously related ones. The MCL model is a sub-model of GMCL, but it is programmed separately because: a) its stand-alone importance; b) different MSE methods are only available for the MCL model; c) extrapolation is not allowed for GMCL. 

Some technical details about the GMCL model. Assume N triangles are available. Denote \eqn{Y_{i,k}=(Y^{(1)}_{i,k}, \ldots ,Y^{(N)}_{i,k})} as an \eqn{N \times 1} vector of  cumulative losses at accident year i and development year k where (n) refers to the n-th triangle. The GMCL model in development period k is:
  \deqn{Y_{i,k+1}=A_k + B_k \cdot Y_{i,k}+\epsilon_{i,k},}
where \eqn{A_k} is a column of intercepts and \eqn{B_k} is the usual development matrix. By default, \code{MultiChainLadder} sets \eqn{A_k} to be zero, but one can specify a model with intercepts using the \code{int} argument.
Assumptions for this model are:
\deqn{E(\epsilon_{i,k}|Y_{i,1},\ldots,Y_{i,I+1-k})=0.}
\deqn{cov(\epsilon_{i,k}|Y_{i,1},\ldots,Y_{i,I+1-k})=\Sigma_{\epsilon_{i,k}}=D(Y_{i,k}^{-\delta/2})\Sigma_k D(Y_{i,k}^{-\delta/2}).}
\deqn{\mbox{losses of different accident years are independent}.}
\deqn{\epsilon_{i,k} \, \mbox{are symmetrically distributed}.}

The GMCL model is very flexible since different parameter restrictions can be specified. It will be equivalent to the MCL model if the model does not have intercepts and the development matrix is restricted to be diagonal. When applied to paid and incurred triangles, it can reflect the development relationship between the two triangles, as described by Quarg and Mack (2004). The full bivariate model is identical to the "double regression" model described by Mack (2003), which is argued by him to be equivalent to the Munich Chain Ladder (MuCL) model. GMCL with intercepts can also help improve model adequacy as described by Barnett and Zehnwirth (2000). 

Currently the  model \code{GMCL} can only work for trapezoid data, and it only allows for estimation method \code{mse.method="Mack"}, while the model \code{MCL} allows extrapolation and the mse method that assumes independence among estimated parameters. The model \code{MCL} under estimation method \code{"OLS"} will be equivalent to separate chain ladders (SCL). When one triangle is specified (as a list),  \code{MCL} is equivalent to \code{MackChainLadder}. 

\code{GMCL} allows different model structures to be specified across the development periods. This is usually achieved through the combination of the \code{int} and \code{restrict.regMat} arguments. \code{int} indicates which periods will have intercepts, and \code{restrict.regMat} allows different parameter restrictions to be specified in a list. 


In using the multivariate method, one often specifies separate chain ladder for later periods to stabilize the estimation. In this case, one can use \code{"["}, defined for class \code{triangles} to split the input data, and use the \code{MultiChainLadder} to fit two models, either \code{MCL} or \code{GMCL}, and join them together using \code{Join2Fits}, which creates an object of class \code{MCLFit} or \code{GMCLFit}. Then methods of \code{predict} and \code{Mse} can be called to produce predictions and mean square errors. The function \code{JoinFitMse} is written to make it easy to construct an object of class \code{MultiChainLadder}, for which a couple of methods are defined to produce statistical results and diagonostic plots. 



}

\value{
  \code{MultiChainLadder} returns an object of class \code{MultiChainLadder}  with the following slots:
  \item{model}{model structure used, either \code{MCL} or \code{GMCL}}
  \item{Triangles}{input triangles of cumulative claims, converted to class \code{triangles}}
  \item{models}{fitted models for each development period, output from the call of \code{systemfit}}
  \item{coefficients}{estimated coefficients from \code{systemfit}. They are put into the matrix format for GMCL}
  \item{coefCov}{estimated variance-covariance matrix returned by \code{systemfit}}
  \item{residCov}{estimated residual covariance matrix returned by \code{systemfit}}
  \item{fit.method}{estimation method}
  \item{delta}{value of delta}
  \item{mse.ay}{mean square error matrix for each accident year}
  \item{mse.ay.est}{estimation error matrix for each accident year}
  \item{mse.ay.proc}{process error matrix for each accident year}
  \item{mse.total}{mean square error matrix for all accident years combined}
  \item{mse.total.est}{estimation error matrix for all accident years combined}
  \item{mse.total.proc}{process error matrix for all accident years combined}
  \item{FullTriangles}{forecasted full triangles of class \code{triangles}}
  \item{int}{intercept indicators}
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

\seealso{See also \code{\link{MackChainLadder}}, \code{\link{MunichChainLadder}}, \code{\linkS4class{triangles}}, \code{\linkS4class{MultiChainLadder}}, \code{\link{summary,MultiChainLadder-method}} and  \code{\link{plot,MultiChainLadder,missing-method}}.
}

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
# This is also a test of different programs to calculate Mse
# Notice that GMCL does not work for triangles that need to be extrapolated

data(liab)

liab <- as(liab,"triangles")  # transform "list" to be "triangles"
liab2 <- liab[,1:10]   # special "[" is defined for class "triangles"

#impose parameter restriction in GMCL so that development matrix is diagonal
coefr <- matrix(0,4,2)     
coefr[1,1] <- coefr[4,2] <- 1
coefr <- rep(list(coefr),9)
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

# Illustrate the Merz and W\"{u}thrich estimator. The unbiased estimator can be specified as
# "Theil" in the systemfit package. This estimator may not be positive semi-definite and 
# is not recommended. 

fit.mw=MultiChainLadder(liab1,mse.method="Independence",
				extrap=FALSE,
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
        print(summary(fit.iter)@report.summary[[3]][15,4:5])
        }

# Reproduce results in Zhang (2009)
# Data auto consists of three triangles, paid personal auto, incurred personal auto
# and paid commercial auto. The paid and incurred triangles from personal auto will
# result in divergent Paid-to-incurred ratios under SCL. MCL is inadequate since the
# residual plots have clear downward pattern. GMCL provides a nice solution here to 
# resolve several problems with SCL and MCL. The following illustrates the use of 
# different models step by step.   

auto <- as(auto,"triangles")

# split data so that SCL is used for years 7:10 in MCL and GMCL
da1 <- auto[,1:7]
da2 <- auto[,7:10]

# MCL actually fails without using SCL in years 8:9

fit.mcl8 <- MultiChainLadder(auto[,8:9],model="MCL",extrap=FALSE)
coef(fit.mcl8)
residCov(fit.mcl8)	

# This is because the input residual covariance is almost singular
\dontrun{
inputCov <- fit.mcl8$models[[1]]$residCovEst
solve(inputCov)
}

#1. fit with SCL 
fit.scl <- MultiChainLadder(auto,"OLS")

## 2. fit with MCL 
fit1<-MultiChainLadder(da1,"SUR",model="MCL",extrap=FALSE)
fit2<-MultiChainLadder(da2,"OLS")
fit <- Join2Fits(fit1,fit2)
pred <- predict(fit)
mse <- Mse(fit,pred)
fit.mcl<- JoinFitMse(fit,mse)

# 3. GMCL with only intercepts
coefr <- matrix(0,12,6)
pos=cbind(c(1,2,5,7,9,12),1:6)
coefr[pos] <- 1		#coefficient restriction matrix
int=1:6				# specify which periods need intercepts
restrict.regMat <- c(rep(list(coefr),6),rep(list(NULL),3))

fit1<-MultiChainLadder(da1,"SUR",
			int=int,
			restrict.regMat=restrict.regMat,
			model="GMCL")
fit2<-MultiChainLadder(da2,"OLS")
fit <- Join2Fits(fit1,fit2)
pred <- predict(fit)
mse <- Mse(fit,pred)
fit.int <- JoinFitMse(fit,mse)


### 4. GMCL fit with full models

coefr <- matrix(0,12,8)
pos=cbind(c(1:3,5:7,9,12),1:8)
coefr[pos] <- 1
int=1:6
restrict.regMat <- c(rep(list(coefr),6),rep(list(NULL),3))

fit1<-MultiChainLadder(da1,"SUR",
			int=int,
			restrict.regMat=restrict.regMat,
			model="GMCL")
fit2<-MultiChainLadder(da2,"OLS")
fit <- Join2Fits(fit1,fit2)
pred <- predict(fit)
mse <- Mse(fit,pred)
fit.full <- JoinFitMse(fit,mse)

# 5. GMCL with parameter selection
# we leave out the detail of the stepwise process 
# and only show the final selected model

# specify differnt restriction matrix for each period
coefr1 <- matrix(0,12,6)
pos1 <- cbind(c(2,3,5,7,9,12),1:6)
coefr2 <- matrix(0,12,5)
pos2 <- cbind(c(2,5:7,12),1:5)
coefr3 <- matrix(0,12,7)
pos3  <- cbind(c(1:2,5:7,9,12),1:7)
coefr4 <- matrix(0,12,7)
pos4 <- cbind(c(1:2,5,6,7,9,12),1:7)
coefr5 <- matrix(0,12,5)
pos5 <- cbind(c(1,2,5,7,12),1:5)
coefr6 <- matrix(0,12,5)
pos6 <- cbind(c(1,2,7,9,12),1:5)
coefr1[pos1] <- 1
coefr2[pos2] <- 1
coefr3[pos3] <- 1
coefr4[pos4] <- 1
coefr5[pos5] <- 1
coefr6[pos6] <- 1

int=1:6
restrict.regMat <- c(list(coefr1,coefr2,coefr3,coefr4,coefr5,coefr6,rep(list(NULL),3)))

fit1<-MultiChainLadder(da1,"SUR",
			int=int,
			restrict.regMat=restrict.regMat,
			model="GMCL")
fit2<-MultiChainLadder(da2,"OLS")
fit <- Join2Fits(fit1,fit2)
pred <- predict(fit)
mse <- Mse(fit,pred)
fit.sel <- JoinFitMse(fit,mse)


# 6. Munich Chain Ladder
fit.mucl=MunichChainLadder(auto[[1]],auto[[2]])


### combine all the paid-to-incurred ratios

u1 <- summary(fit.scl)@Ultimate
r1 <- u1[,1]/u1[,2]
u2 <- summary(fit.mcl)@Ultimate
r2 <- u2[,1]/u2[,2]
u3 <- summary(fit.int)@Ultimate
r3 <- u3[,1]/u3[,2]
u4 <- summary(fit.full)@Ultimate
r4 <- u4[,1]/u4[,2]
u5 <- summary(fit.sel)@Ultimate
r5 <- u5[,1]/u5[,2]
r6=summary(fit.mucl)[[1]][,6]
r6 <- c(r6,summary(fit.mucl)[[2]][2,3])

ratios <- cbind(r1,r2,r3,r6,r4,r5)
dimnames(ratios)[[2]] <- c("SCL","MCL","GMCL1","MuCL","GMCL2","GMCL3" )
ratios=format(round(ratios*100,2), big.mark=",", scientific=FALSE) 
print(ratios,quote = FALSE)


### Estimated parameters
fit.sel@coefficients
fit.sel@residCov
summary(fit.sel)@residCor

## summary statistics
summary(fit.sel,portfolio="1+3")@report.summary[[4]]



\dontrun{
### residual plots
x11(4,10)
par(mfrow=c(2,3))
plot(fit.scl,which.plot=3:4)
plot(fit.mcl,which.plot=3:4)
plot(fit.int,which.plot=3:4)
plot(fit.full,which.plot=3:4)
plot(fit.sel,which.plot=3:4)

# histogram plots
r <- summary(fit.sel)@rstandard
par(mfrow=c(1,3))
for (i in 1:3){
	hist(r[,i],20,freq=FALSE,xlim=c(-2,2),
	xlab="Standardized Residuals",
	main=paste("Histogram for Triangle",i))
	lines(density(r[,i]))
}
}



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

