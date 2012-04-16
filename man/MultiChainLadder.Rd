\name{MultiChainLadder}
\alias{MultiChainLadder}
\alias{MultiChainLadder2}

\title{ Multivariate Chain Ladder Models }
\description{
  The function \code{MultiChainLadder} implements multivariate methods to  forecast insurance loss payments based on several cumulative claims development triangles. These methods are multivariate extensions of the chain ladder technique, which develop several correlated triangles simultaneously in a way that both contemporaneous correlations and structural relationships can be accounted for. The estimated conditional Mean Square Errors (MSE) are also produced. 
}
\usage{
MultiChainLadder(Triangles, fit.method = "SUR", delta = 1,
  int = NULL, restrict.regMat = NULL, extrap = TRUE, 
  mse.method = "Mack", model = "MCL", ...)
    
MultiChainLadder2(Triangles, mse.method = "Mack", last = 3, 
  type = c("MCL", "MCL+int", "GMCL-int", "GMCL"), ...)    
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangles}{a list of cumulative claims triangles of the same dimensions. }
  \item{fit.method}{the method used to fit the multivariate regression in each development period. The default is \code{"SUR"} - seemingly unrelated regressions. When \code{"OLS"} (Ordinary Least Squares) is used, this is the same as developing each triangle separately. } 
  \item{delta}{parameter for controlling weights. It is used to determine the covariance structure \eqn{D(Y_{i,k}^{\delta/2})\Sigma_k D(Y_{i,k}^{\delta/2})}. The default value \code{1} means that the variance is proportional to the cumulative loss from the previous period. }   
  \item{int}{a numeric vector that indicates which development periods have intercepts specified. This only takes effect for \code{model = "GMCL"}. The default \code{NULL} means that no intercepts are specified.}   
  \item{restrict.regMat}{a list of matrix specifying parameter restriction matrix for each period. This is only used for \code{model = "GMCL"}. The default value \code{NULL} means no restriction is imposed on the development matrix. For example, if there are 3 triangles, there will be 9 parameters in the development matrix for each period if \code{restrict.regMat = NULL}. See \code{systemfit} for how to specify the appropriate parameter constraints.} 
  \item{extrap}{a logical value indicating whether to use Mack's extrapolation method for the last period to get the residual variance  estimation. It only takes effect for  \code{model = "MCL"}. If the data are  trapezoids, it is set to be \code{FALSE} automatically and a warning message is given.}
  \item{mse.method}{method to estimate the mean square error. It can be either \code{"Mack"} or \code{"Independence"}, which are  the multivariate generalization of Mack's formulas and the conditional re-sampling approach, respectively.}
  \item{model}{the structure of the model to be fitted. It is either \code{"MCL"} or \code{"GMCL"}. See details below. }
  
  \item{last}{an integer. The \code{MultiChainLadder2} function splits the triangles into 2 parts internally (see details below), and the \code{last} argument indicates how many of the development periods in the tail go into the second part of the split. The default is \code{3}.}
  
  \item{type}{the type of the model structure to be specified for the first part of the split model in \code{MultiChainLadder2}. \code{"MCL"}- the multivariate chain ladder with diagonal development matrix; \code{"MCL+int"}- the multivariate chain ladder with additional intercepts; \code{"GMCL-int"}- the general multivariate chain ladder without intercepts; and \code{"GMCL"} - the full general multivariate chain ladder with intercepts and non-diagonal development matrix. }
  \item{\dots}{arguments passed to \code{systemfit}.}

}

\details{
This function implements multivariate loss reserving models within the chain ladder framework.  Two major models are included. One is the Multivariate Chain Ladder (MCL) model proposed by Prohl and Schmidt (2005). This is a direct multivariate generalization of the univariate chain ladder model in that losses from different triangles are assumed to be correlated but the mean development in one triangle only depends on its past values, not on the observed values from other triangles. In contrast, the other model, the General Multivariate Chain Ladder (GMCL) model outlined in Zhang (2010), extends the MCL model by allowing development dependencies among different triangles as well as the inclusion of regression intercepts. As a result, structurally related triangles, such as the  paid and incurred loss triangles or the paid loss and case reserve triangles, can be developed together while still accounting for the potential contemporaneous correlations. While the MCL model is a special case of the GMCL model, it is programmed and listed separately because: a) it is an important model for its own sake; b) different MSE methods are only available for the MCL model; c) extrapolation of the residual variance estimation can be implemented for the MCL model, which is considerably difficult for the GMCL model. 

We introduce some details of the GMCL model in the following. Assume N triangles are available. Denote \eqn{Y_{i,k}=(Y^{(1)}_{i,k}, \ldots ,Y^{(N)}_{i,k})} as an \eqn{N \times 1} vector of  cumulative losses at accident year i and development year k, where (n) refers to the n-th triangle. The GMCL model in development period k (from development year k to year k+1) is:
  \deqn{Y_{i,k+1}=A_k + B_k \cdot Y_{i,k}+\epsilon_{i,k},}
where \eqn{A_k} is a column of intercepts and \eqn{B_k} is the  \eqn{N \times N} development matrix. By default, \code{MultiChainLadder} sets \eqn{A_k} to be zero. This behavior can be changed by appropriately specifying the \code{int} argument.
Assumptions for this model are:
\deqn{E(\epsilon_{i,k}|Y_{i,1},\ldots,Y_{i,I+1-k})=0.}
\deqn{cov(\epsilon_{i,k}|Y_{i,1},\ldots,Y_{i,I+1-k})=\Sigma_{\epsilon_{i,k}}=D(Y_{i,k}^{\delta/2})\Sigma_k D(Y_{i,k}^{\delta/2}).}
\deqn{\mbox{losses of different accident years are independent}.}
\deqn{\epsilon_{i,k} \, \mbox{are symmetrically distributed}.}

The GMCL model structure is generally over-parameterized. Parameter restrictions are usually necessary for the estimation to be feasible, which can be specified through the \code{restrict.regMat} argument. We refer the users to the documentation for \code{\link[systemfit]{systemfit}} for details and the demo of the present function for examples. 

In particular, if one restricts the development matrix to be diagonal, the GMCL model will reduce to the MCL model. When non-diagonal development matrix is used and the GMCL model is applied to paid and incurred loss triangles, it can reflect the development relationship between the two triangles, as described in Quarg and Mack (2004). The full bivariate model is identical to the "double regression" model described by Mack (2003), which is argued by him to be very similar to the Munich Chain Ladder (MuCL) model. The GMCL model with intercepts can also help improve model adequacy as described in Barnett and Zehnwirth (2000). 

Currently, the GMCL model only works for trapezoid data, and only implements \code{mse.method = "Mack"}. The MCL model allows an additional mse estimation method that assumes independence among the estimated parameters. Further, the MCL model using \code{fit.method = "OLS"} will be equivalent to running univariate chain ladders separately on each triangle. Indeed, when only one triangle is specified (as a list),  the MCL model is equivalent to \code{MackChainLadder}. 

The GMCL model allows different model structures to be specified in each development period. This is generally achieved through the combination of the \code{int} argument, which specifies the periods that have intercepts, and the \code{restrict.regMat} argument, which imposes parameter restrictions on the development matrix. 


In using the multivariate methods, we often specify separate univariate chain ladders for the tail periods to stabilize the estimation - there are few data points in the tail and running a multivariate model often produces extremely volatile estimates or even fails. In this case, we can use the subset operator \code{"["} defined for class \code{triangles} to split the input data into two parts. We can specify a multivariate model with rich structures on the first part to reflect the multivariate dependencies, and simply apply multiple univariate chain ladders on the second part. The two models are subsequently joined together using the \code{Join2Fits} function. We can then invoke the \code{predict} and \code{Mse} methods to produce loss predictions and mean square error estimations. They can further be combined via the \code{JoinFitMse} function  to construct an object of class \code{MultiChainLadder}. See the demo for such examples. 

To facilitate such a split-and-join process for most applications, we have created the function \code{MultiChainLadder2}. This function splits the data according to the \code{last} argument (e.g., if \code{last = 3}, the last three periods go into the second part), and fits the first part according to the structure indicated in the \code{type} argument. See the 'Arguments' section for details. 

}

\note{
When \code{MultiChainLadder} or \code{MultiChainLadder2} fails, the most possible reason is that there is little or no development in the tail periods. That is, the development factor is \code{1} or almost equal to \code{1}. In this case, the \text{systemfit} function may fail even for \code{fit.method = "OLS"}, because the residual covariance matrix \eqn{\Sigma_k} is singular. The simplest solution is to remove these columns using the \code{"["} operator and fit the model on the remaining part. 

Also, we recommend the use of \code{MultiChainLadder2} over \code{MultiChainLadder}. The function \code{MultiChainLadder2} meets the need for most applications, is relatively easy to use and produces more stable but very similar results to \code{MultiChainLadder}. Use \code{MultiChainLadder} only when non-standard situation arises, e.g., when different parameter restrictions are needed for different periods. See the demo for such examples.   
}

\value{
  \code{MultiChainLadder} returns an object of class \code{MultiChainLadder}  with the following slots:
  \item{model}{the model structure used, either \code{"MCL"} or \code{"GMCL"}}
  \item{Triangles}{input triangles of cumulative claims that are converted to class \code{triangles} internally. }
  \item{models}{fitted models for each development period. This is the output from the call of \code{systemfit}.}
  \item{coefficients}{estimated regression coefficients or development parameters. They are put into the matrix format for the GMCL model.}
  \item{coefCov}{estimated variance-covariance matrix for the regression coefficients.}
  \item{residCov}{estimated residual covariance matrix.}
  \item{fit.method}{multivariate regression estimation method}
  \item{delta}{the value of delta}
  \item{mse.ay}{mean square error matrix for each accident year}
  \item{mse.ay.est}{estimation error matrix for each accident year}
  \item{mse.ay.proc}{process error matrix for each accident year}
  \item{mse.total}{mean square error matrix for all accident years combined}
  \item{mse.total.est}{estimation error matrix for all accident years combined}
  \item{mse.total.proc}{process error matrix for all accident years combined}
  \item{FullTriangles}{the forecasted full triangles of class \code{triangles}}
  \item{int}{intercept indicators}
}
\references{ 
\cite{Buchwalder M, Buhlmann H, Merz M, Wuthrich M.V (2006). The mean square error of prediction in the chain ladder reserving method (Mack and Murphy revisited), \emph{ASTIN Bulletin}, 36(2), 521-542.}

\cite{Prohl C, Schmidt K.D (2005). Multivariate chain-ladder, \emph{Dresdner Schriften zur Versicherungsmathematik}.}

\cite{ Mack T (1993). Distribution-free calculation of the standard error, \emph{ASTIN Bulletin}, 23, No.2. }

\cite{ Mack T (1999). The standard error of chain ladder reserve estimates: recursive calculation and inclusion of a tail factor, \emph{ASTIN Bulletin}, 29, No.2, 361-366.}

\cite{ Merz M, Wuthrich M (2008). Prediction error of the multivariate chain ladder reserving method, \emph{North American Actuarial Journal}, 12, No.2, 175-197.}

\cite{Zhang Y (2010). A general multivariate chain ladder model.\emph{Insurance: Mathematics and Economics}, 46, pp. 588-599.}

\cite{Zhang Y (2010). Prediction error of the general multivariate chain ladder model.}

 }

\author{ Wayne Zhang \email{actuary_zhang@hotmail.com}} 

\seealso{See also \code{\link{MackChainLadder}}, \code{\link{MunichChainLadder}}, \code{\linkS4class{triangles}}, \code{\linkS4class{MultiChainLadder}}, \code{\link{summary,MultiChainLadder-method}} and  \code{\link{plot,MultiChainLadder,missing-method}}.
}

\examples{

# This shows that the MCL model using "OLS" is equivalent to 
# the MackChainLadder when applied to one triangle 

data(GenIns)
(U1 <- MackChainLadder(GenIns, est.sigma = "Mack"))
(U2 <- MultiChainLadder(list(GenIns), fit.method = "OLS"))

# show plots 
parold <- par(mfrow = c(2, 2))
plot(U2, which.plot = 1:4)
plot(U2, which.plot = 5)
par(parold)

# For mse.method = "Independence", the model is equivalent 
# to that in Buchwalder et al. (2006)

(B1 <- MultiChainLadder(list(GenIns), fit.method = "OLS", 
    mse.method = "Independence"))

# use the unbiased residual covariance estimator 
# in Merz and Wuthrich (2008)
(W1 <- MultiChainLadder2(liab, mse.method = "Independence", 
    		control = systemfit.control(methodResidCov = "Theil"))) 

# use the iterative residual covariance estimator
for (i in 1:5){
  W2 <- MultiChainLadder2(liab, mse.method = "Independence", 
      control = systemfit.control(methodResidCov = "Theil", maxiter = i))
  print(format(summary(W2)@report.summary[[3]][15, 4:5], 
          digits = 6, big.mark = ","))    
}

# The following fits an MCL model with intercepts for years 1:7
# and separate chain ladder models for the rest periods
f1 <- MultiChainLadder2(auto, type = "MCL+int")

# compare with the model without intercepts through residual plots
f0 <- MultiChainLadder2(auto, type = "MCL")

parold <- par(mfrow = c(2, 3), mar = c(3, 3, 2, 1))
mt <- list(c("Personal Paid", "Personal Incured", "Commercial Paid"))
plot(f0, which.plot = 3, main = mt)
plot(f1, which.plot = 3, main = mt)
par(parold)

## summary statistics
summary(f1, portfolio = "1+3")@report.summary[[4]]

# model for joint development of paid and incurred triangles 
da <- auto[1:2]
# MCL with diagonal development
M0 <- MultiChainLadder(da)
# non-diagonal development matrix with no intercepts
M1 <- MultiChainLadder2(da, type = "GMCL-int")
# Munich Chain Ladder
M2 <- MunichChainLadder(da[[1]], da[[2]])
# compile results and compare projected paid to incurred ratios
r1 <- lapply(list(M0, M1), function(x){
          ult <- summary(x)@Ultimate
          ult[, 1] / ult[, 2]
      })
names(r1) <- c("MCL", "GMCL")
r2 <- summary(M2)[[1]][, 6]
r2 <- c(r2, summary(M2)[[2]][2, 3])
print(do.call(cbind, c(r1, list(MuCl = r2))) * 100, digits = 4)
# To reproduce results in Zhang (2010) and see more examples, use:
\dontrun{
 demo(MultiChainLadder)
}

}


\keyword{ models }

