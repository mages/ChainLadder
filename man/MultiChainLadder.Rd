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

\cite{Zhang Y (2010). A general multivariate chain ladder model.}

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
			fit.method="OLS")
summary(uni1)
summary(uni2)

# show plots 
\dontrun{
par(mfrow=c(2,2))
plot(uni2,which.plot=1:4)
plot(uni2,which.plot=5)
}

# This illustrates the use of the "Independence" assumption in 
# calculating the Mse, which is equivalent to the result in BBMW

fit.bbmw <- MultiChainLadder(list(GenIns),
				fit.method="OLS", 
				mse.method="Independence")
fit.bbmw


# The following shows the inclusion of intercepts for years 1:7
auto <- as(auto,"triangles")

da1 <- auto[,1:7]
da2 <- auto[,7:10]

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

## summary statistics
summary(fit.int,portfolio="1+3")@report.summary[[4]]

\dontrun{
### residual plots
par(mfrow=c(2,3))
plot(fit.int,which.plot=3:4)
}

# To reproduce results in Zhang (2010) and see more examples, use:
\dontrun{
 demo(MackChainLadder)
}

}


\keyword{ models }

