\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,MultiChainLadder-method}

\title{ Methods for Function summary}
\description{
 Methods for function \code{summary} to calculate summary statistics from a "MultiChainLadder" object. 
}


\usage{

\S4method{summary}{MultiChainLadder}(object, portfolio=NULL,...)
}

\arguments{
  \item{object}{object of class \code{"MultiChainLadder"}}
  \item{portfolio}{character strings specifying which triangles to be summed up as portfolio.  }
  \item{\dots}{ optional arguments to \code{summary} methods }
}
\details{

 \code{summary} calculations the summary statistics for each triangle and the whole portfolio from \code{portfolio}. \code{portfolio} defaults to the sum of all input triangles. It can also be specified as "i+j" format, which means the sum of the i-th and j-th triangle as portfolio. For example, \code{"1+3"} means the sum of the first and third triangle as portfolio.
  
}
\value{ The \code{summary} function returns an object of class "MultiChainLadderSummary" that has the following slots:
  \item{Triangles}{input triangles}
  \item{FullTriangles}{predicted triangles}
  \item{S.E.Full}{a list of prediction errors for each cell}
  \item{S.E.Est.Full}{a list of estimation errors for each cell}
  \item{S.E.Proc.Full}{a list of process errors for each cell}
  \item{Ultimate}{predicted ultimate losses for each triangle and portfolio  }
  \item{Latest}{ latest observed losses for each triangle and portfolio  }
  \item{IBNR}{ predicted IBNR for each triangle and portfolio  }
  \item{S.E.Ult}{ a matrix of prediction errors of ultimate losses for each triangle and portfolio  }
  \item{S.E.Est.Ult}{ a matrix of estimation errors of ultimate losses for each triangle and portfolio  }
  \item{S.E.Proc.Ult}{ a matrix of process errors of ultimate losses for each triangle and portfolio  }
  \item{report.summary}{ summary statistics for each triangle and portfolio  }
  \item{coefficients}{estimated coefficients from \code{systemfit}. They are put into the matrix format for GMCL}
  \item{coefCov}{estimated variance-covariance matrix returned by \code{systemfit}}
  \item{residCov}{estimated residual covariance matrix returned by \code{systemfit}}
  \item{rstandard}{ standardized residuals  }
  \item{fitted.values}{ fitted.values  }
  \item{residCor}{ residual correlation  }
  \item{model.summary}{ summary statistics for the cofficients including p-values }
  \item{portfolio}{how portfolio is calculated}
}

\author{Wayne (Yanwei) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See Also \code{\link{MultiChainLadder}} }
\examples{
data(GenIns)
fit.bbmw=MultiChainLadder(list(GenIns),fit.method="OLS", mse.method="Independence")
summary(fit.bbmw)

}

\keyword{methods}

