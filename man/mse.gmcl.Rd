\name{mse.gmcl}
\alias{mse.mcl}
\alias{mse.gmcl}

\title{ Conditonal Mean Square Error }
\description{
  These functions calculate the conditional mean square errors of the GMCL or MCL models. 
}
\usage{
mse.gmcl(models,fullTriangles)

mse.mcl(mse.method="Mack", models,fullTriangles )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{models}{object from calling the fit.gmcl or fit.mcl functions} 
  \item{fullTriangles}{ completed triangles from the predict function call} 
  \item{mse.method}{ method to estimate the mean square error. Either "Mack" or "Murphy". } 
}

\details{
  These functions calculates the conditional mean square errors using the recursive formulas in Zhang (2010). In the GMCL model, the conditional  mean square error for single accident years and aggregated accident years are calcualted as:

\deqn{\hat{mse}(\hat{Y}_{i,k+1}|D)=\hat{B}_k \hat{mse}(\hat{Y}_{i,k}|D) \hat{B}_k + (\hat{Y}_{i,k}' \otimes I) \hat{\Sigma}_{B_k} (\hat{Y}_{i,k} \otimes I) +  \hat{\Sigma}_{\epsilon_{i_k}}.}

\deqn{\hat{mse}(\sum^I_{i=a_k}\hat{Y}_{i,k+1}|D)=\hat{B}_k \hat{mse}(\sum^I_{i=a_k+1}\hat{Y}_{i,k}|D) \hat{B}_k + (\sum^I_{i=a_k}\hat{Y}_{i,k}' \otimes I) \hat{\Sigma}_{B_k} (\sum^I_{i=a_k}\hat{Y}_{i,k} \otimes I) +  \sum^I_{i=a_k}\hat{\Sigma}_{\epsilon_{i_k}} .}

In the MCL model, the conditional  mean square error for single accident years and aggregated accident years from the Murphy formulas are calcualted as:


\deqn{\hat{mse}(\hat{Y}_{i,k+1}|D)=(\hat{\beta}_k \hat{\beta}_k') \odot \hat{mse}(\hat{Y}_{i,k}|D)  + \hat{\Sigma}_{\beta_k} \odot (\hat{Y}_{i,k} \hat{Y}_{i,k}') +  \hat{\Sigma}_{\epsilon_{i_k}} +\hat{\Sigma}_{\beta_k} \odot \hat{mse}^E(\hat{Y}_{i,k}|D) .}

\deqn{\hat{mse}(\sum^I_{i=a_k}\hat{Y}_{i,k+1}|D)=(\hat{\beta}_k \hat{\beta}_k') \odot \sum^I_{i=a_k+1}\hat{mse}(\hat{Y}_{i,k}|D)  + \hat{\Sigma}_{\beta_k} \odot (\sum^I_{i=a_k}\hat{Y}_{i,k} \sum^I_{i=a_k}\hat{Y}_{i,k}') +  \sum^I_{i=a_k}\hat{\Sigma}_{\epsilon_{i_k}} \cr
 +\hat{\Sigma}_{\beta_k} \odot \sum^I_{i=a_k}\hat{mse}^E(\hat{Y}_{i,k}|D) .}

For the Mack approach, the cross-product term \eqn{\hat{\Sigma}_{\beta_k} \odot \hat{mse}^E(\hat{Y}_{i,k}|D) }in the above two formulas will drop out.

}

\value{
  \code{mse.mcl} or \code{mse.gmcl}  returns a list with the following elements
  \item{mse.ay}{condtional mse for each accdient year}
  \item{mse.ay.est}{conditional estimation mse for each accdient year}
  \item{mse.ay.proc}{conditional process mse for each accdient year}
  \item{mse.total}{condtional mse for aggregated accdient years}
  \item{mse.total.est}{conditional estimation mse for aggregated accdient years}
  \item{mse.total.proc}{conditional process mse for aggregated accdient years}
  \item{fullTriangles}{completed triangles}
  \item{fit.method}{method to estimate mse. Only for \code{mcl} object. }

}
\references{
\cite{Zhang Y (2009). A general multivariate chain ladder model.}

\cite{Zhang Y (2010). Prediction error of multivariate reserving models in the chain ladder framework.}
}
\author{Yanwei (Wayne) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See also \link{mcl}, \link{gmcl}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
