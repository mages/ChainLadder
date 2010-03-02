\name{Mse-methods}
\docType{genericFunction}
\alias{Mse}
\alias{Mse-methods}
\alias{Mse,GMCLFit,triangles-method}
\alias{Mse,MCLFit,triangles-method}

\title{ Methods for Generic Function Mse }
\description{
  \code{Mse} is a generic function to calculate mean square error estimations in the chain ladder framework. 
}

\usage{
Mse(ModelFit, FullTriangles, ...)

\S4method{Mse}{GMCLFit,triangles}(ModelFit, FullTriangles, ...)
\S4method{Mse}{MCLFit,triangles}(ModelFit, FullTriangles, mse.method="Mack", ...)
}


\arguments{
  \item{ModelFit}{An object of class "GMCLFit" or "MCLFit". }
  \item{FullTriangles}{An object of class "triangles". Should be the output from a call of \code{predict}. }
  \item{mse.method}{Character strings that specify the MSE estimation method. Only works for "MCLFit". Use \code{"Mack"} for the generazliation of the Mack (1993) approach, and \code{"Independence"} for the generalization of the Murphy (1994) and BBMW (2006) approach.   }
  \item{\dots}{ Currently not used. }
  }

\details{
  These functions calculate the conditional mean square errors using the recursive formulas in Zhang (2010). In the GMCL model, the conditional  mean square error for single accident years and aggregated accident years are calcualted as:

\deqn{\hat{mse}(\hat{Y}_{i,k+1}|D)=\hat{B}_k \hat{mse}(\hat{Y}_{i,k}|D) \hat{B}_k + (\hat{Y}_{i,k}' \otimes I) \hat{\Sigma}_{B_k} (\hat{Y}_{i,k} \otimes I) +  \hat{\Sigma}_{\epsilon_{i_k}}.}

\deqn{\hat{mse}(\sum^I_{i=a_k}\hat{Y}_{i,k+1}|D)=\hat{B}_k \hat{mse}(\sum^I_{i=a_k+1}\hat{Y}_{i,k}|D) \hat{B}_k + (\sum^I_{i=a_k}\hat{Y}_{i,k}' \otimes I) \hat{\Sigma}_{B_k} (\sum^I_{i=a_k}\hat{Y}_{i,k} \otimes I) +  \sum^I_{i=a_k}\hat{\Sigma}_{\epsilon_{i_k}} .}

In the MCL model, the conditional  mean square error for single accident years and aggregated accident years from the Murphy&BBMW formulas are calcualted as:


\deqn{\hat{mse}(\hat{Y}_{i,k+1}|D)=(\hat{\beta}_k \hat{\beta}_k') \odot \hat{mse}(\hat{Y}_{i,k}|D)  + \hat{\Sigma}_{\beta_k} \odot (\hat{Y}_{i,k} \hat{Y}_{i,k}') +  \hat{\Sigma}_{\epsilon_{i_k}} +\hat{\Sigma}_{\beta_k} \odot \hat{mse}^E(\hat{Y}_{i,k}|D) .}

\deqn{\hat{mse}(\sum^I_{i=a_k}\hat{Y}_{i,k+1}|D)=(\hat{\beta}_k \hat{\beta}_k') \odot \sum^I_{i=a_k+1}\hat{mse}(\hat{Y}_{i,k}|D)  + \hat{\Sigma}_{\beta_k} \odot (\sum^I_{i=a_k}\hat{Y}_{i,k} \sum^I_{i=a_k}\hat{Y}_{i,k}') +  \sum^I_{i=a_k}\hat{\Sigma}_{\epsilon_{i_k}} 
 +\hat{\Sigma}_{\beta_k} \odot \sum^I_{i=a_k}\hat{mse}^E(\hat{Y}_{i,k}|D) .}

For the Mack approach, the cross-product term \eqn{\hat{\Sigma}_{\beta_k} \odot \hat{mse}^E(\hat{Y}_{i,k}|D) }in the above two formulas will drop out.

}

\value{
  \code{Mse}  returns an object of class "MultiChainLadderMse" that has  the following elements:
  \item{mse.ay}{condtional mse for each accdient year}
  \item{mse.ay.est}{conditional estimation mse for each accdient year}
  \item{mse.ay.proc}{conditional process mse for each accdient year}
  \item{mse.total}{condtional mse for aggregated accdient years}
  \item{mse.total.est}{conditional estimation mse for aggregated accdient years}
  \item{mse.total.proc}{conditional process mse for aggregated accdient years}
  \item{FullTriangles}{completed triangles} 

}
\references{
\cite{Zhang Y (2009). A general multivariate chain ladder model.}

\cite{Zhang Y (2010). Prediction error of multivariate reserving models in the chain ladder framework.}
}
\author{Wayne (Yanwei) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See also \code{\link{MultiChainLadder}.}
}
\keyword{methods}
\keyword{models}
