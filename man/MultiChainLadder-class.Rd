\name{MultiChainLadder-class}
\docType{class}
\alias{MultiChainLadder-class}
\alias{$,MultiChainLadder-method}
\alias{[[,MultiChainLadder,numeric,missing-method}
\alias{[[,MultiChainLadder,character,missing-method}
\alias{coef,MultiChainLadder-method}
\alias{fitted.values,MultiChainLadder-method}
\alias{fitted,MultiChainLadder-method}
\alias{names,MultiChainLadder-method}
\alias{residuals,MultiChainLadder-method}
\alias{resid,MultiChainLadder-method}
\alias{rstandard,MultiChainLadder-method}
\alias{show,MultiChainLadder-method}
\alias{vcov,MultiChainLadder-method}

\title{Class "MultiChainLadder" of Multivariate Chain Ladder Results }
\description{ This class includes the first and second moment estimation result using the multivariate reserving methods in chain ladder. Several primitive methods and statistical methods are also created to facilitate further analysis. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MultiChainLadder", ...)}, or they could also be a result of calls from \code{MultiChainLadder} or \code{JoinFitMse}.

}
\section{Slots}{
	 \describe{
    \item{\code{model}:}{Object of class \code{"character"}. Either "MCL" or "GMCL".  }
    \item{\code{Triangles}:}{Object of class \code{"triangles"}. Input triangles. }
    \item{\code{models}:}{Object of class \code{"list"}. Fitted regression models using \code{systemfit}.  }
    \item{\code{coefficients}:}{Object of class \code{"list"}. Estiamted regression coefficients.  }
    \item{\code{coefCov}:}{Object of class \code{"list"}. Estimated variance-covariance matrix of coefficients. }
    \item{\code{residCov}:}{Object of class \code{"list"}. Estimated residual covariance matrix.  }
    \item{\code{fit.method}:}{Object of class \code{"character"}. Could be values of "SUR" or "OLS".  }
    \item{\code{delta}:}{Object of class \code{"numeric"}. Parameter for weights.  }
    \item{\code{int}:}{Object of class \code{"NullNum"}. Indicator of which peroids have intercepts.  }
    \item{\code{mse.ay}:}{Object of class \code{"matrix"}. Conditional mse for each accdient year. }
    \item{\code{mse.ay.est}:}{Object of class \code{"matrix"}. Conditional estimation mse for each accdient year.  }
    \item{\code{mse.ay.proc}:}{Object of class \code{"matrix"}. Conditional process mse for each accdient year.  }
    \item{\code{mse.total}:}{Object of class \code{"matrix"}. Conditional mse for aggregated accdient years.  }
    \item{\code{mse.total.est}:}{Object of class \code{"matrix"}. Conditional estimation mse for aggregated accdient years.  }
    \item{\code{mse.total.proc}:}{Object of class \code{"matrix"}. Conditional process mse for aggregated accdient years.  }
    \item{\code{FullTriangles}:}{Object of class \code{"triangles"}. Completed triangles.  }
    \item{\code{restrict.regMat}:}{Object of class \code{"NullList"}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{MultiChainLadderFit}"}, directly.
Class \code{"\linkS4class{MultiChainLadderMse}"}, directly.
}
\section{Methods}{
  \describe{
    \item{$}{\code{signature(x = "MultiChainLadder")}: Method for primitive function \code{"$"}. It extracts a slot of \code{x} with a specified slot name, just as in list. }
    \item{[[}{\code{signature(x = "MultiChainLadder", i = "numeric", j = "missing")}:  Method for primitive function \code{"[["}. It extracts the i-th slot of a \code{"MultiChainLadder"} object, just as in list. \code{i} could be a vetor. }
    \item{[[}{\code{signature(x = "MultiChainLadder", i = "character", j = "missing")}: Method for primitive function \code{"[["}. It extracts the slots of a \code{"MultiChainLadder"} object with names in \code{i}, just as in list. \code{i} could be a vetor. }
    \item{coef}{\code{signature(object = "MultiChainLadder")}: Method for function \code{coef},  to extract the estimated development matrix. The output is a list.  }
    \item{fitted.values}{\code{signature(object = "MultiChainLadder")}:  Method for function \code{fitted.values},  to calculate the fitted values in the orignal triangles. Note that the return value is a list of fitted valued based on the orignal scale, not the model scale which is first divided by \eqn{Y_{i,k}^{\delta/2}}.  }
    \item{fitted}{\code{signature(object = "MultiChainLadder")}: Same as \code{fitted.values} in the above. }
    \item{names}{\code{signature(x = "MultiChainLadder")}: Method for function \code{names}, which returns the slot names of a \code{"MultiChainLadder"} object. }
    \item{plot}{\code{signature(x = "MultiChainLadder", y = "missing")}: See \code{\link{plot,MultiChainLadder,missing-method}}.  }
    \item{residCov}{\code{signature(object = "MultiChainLadder")}: S4 generic function and method to extract residual covariance from a \code{"MultiChainLadder"} object. }
    \item{residCor}{\code{signature(object = "MultiChainLadder")}: S4 generic function and method to extract residual correlation from a \code{"MultiChainLadder"} object. }
    \item{residuals}{\code{signature(object = "MultiChainLadder")}: Method for function \code{residuals}, to extract residuals  from a system of regression equations. These residuals are based on model scale, and will not be equivalent to those on the original scale if \eqn{\delta} is not set to be 0. One should use \code{rstandard} instead, which is independent of the scale.  }
    \item{resid}{\code{signature(object = "MultiChainLadder")}: Same as \code{residuals}.  }
    \item{rstandard}{\code{signature(model = "MultiChainLadder")}: S4 generic function and method to extract standardized residuals from a \code{"MultiChainLadder"} object.  }
    \item{show}{\code{signature(object = "MultiChainLadder")}: Method for \code{show}. }
    \item{summary}{\code{signature(object = "MultiChainLadder")}: See \code{\link{summary,MultiChainLadder-method}}. }
    \item{vcov}{\code{signature(object = "MultiChainLadder")}: Method for function \code{vcov}, to extract the variance-covariance matrix of a \code{"MultiChainLadder"} object. Note that the result is a list of \code{Bcov}, that is the variance-covariance matrix of the vectorized \eqn{B}. }
	 }
}

\author{ Wayne Zhang \email{actuary_zhang@hotmail.com} }
\seealso{
	See also \code{\link{MultiChainLadder}},\code{\link{summary,MultiChainLadder-method}} and  \code{\link{plot,MultiChainLadder,missing-method}}.   
}
\examples{
# example for class "MultiChainLadder"
data(liab)
fit.liab <-  MultiChainLadder(Triangles = liab)
fit.liab

names(fit.liab)
fit.liab[[1]]
fit.liab$model
fit.liab@model

do.call("rbind",coef(fit.liab))
vcov(fit.liab)[[1]]
residCov(fit.liab)[[1]]
head(do.call("rbind",rstandard(fit.liab)))

}
\keyword{classes}
