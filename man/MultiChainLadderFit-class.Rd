\name{MultiChainLadderFit-class}
\docType{class}
\alias{MultiChainLadderFit-class}
\alias{MCLFit-class}
\alias{GMCLFit-class}
\alias{predict,MCLFit-method}
\alias{predict,GMCLFit-method}

\title{Class "MultiChainLadderFit", "MCLFit" and "GMCLFit"  }
\description{"MultiChainLadderFit" is a virtual class for the fitted models in the multivariate chain ladder reserving framework, "MCLFit" is a result from the interal call \code{.FitMCL} to store results in model \code{MCL} and "GMCLFit" is a result from the interal call \code{.FitGMCL} to store results in model \code{GMCL}. The two classes "MCLFit" and "GMCLFit" differ only in the presentation of \eqn{B_k} and \eqn{\Sigma_{B_k}}, and different methods of \code{Mse} and \code{predict} will be dispatched according to these classes.  }
\section{Objects from the Class}{"MultiChainLadderFit" is a virtual Class: No objects may be created from it. For "MCLFit" and "GMCLFit", objects can be created by calls of the form \code{new("MCLFit", ...)} and \code{new("GMCLFit", ...)} respectively. }
\section{Slots}{
	 \describe{
    \item{\code{Triangles}:}{Object of class \code{"triangles"}  }
    \item{\code{models}:}{Object of class \code{"list"}  }
    \item{\code{B}:}{Object of class \code{"list"}  }
    \item{\code{Bcov}:}{Object of class \code{"list"}  }
    \item{\code{ecov}:}{Object of class \code{"list"}  }
    \item{\code{fit.method}:}{Object of class \code{"character"}  }
    \item{\code{delta}:}{Object of class \code{"numeric"}  }
  }
}

\section{Extends}{
"MCLFit" and "GMCLFit" extends class \code{"\linkS4class{MultiChainLadderFit}"}, directly.
}

\section{Methods}{
No methods defined with class "MultiChainLadderFit" in the signature.

For "MCLFit", the following methods are defined:
 \describe{
    \item{\code{Mse}}{\code{signature(ModelFit = "MCLFit", FullTriangles = "triangles")}: Calculate Mse estimations. }
    \item{\code{predict}}{\code{signature(object = "MCLFit")}: Predict ultimate losses and complete the triangles. The output is an object of class "triangles".}
	 }

For "GMCLFit", the following methods are defined:
  \describe{
    \item{\code{Mse}}{\code{signature(ModelFit = "GMCLFit", FullTriangles = "triangles")}: Calculate Mse estimations. }
    \item{\code{predict}}{\code{signature(object = "GMCLFit")}: Predict ultimate losses and complete the triangles. The output is an object of class "triangles". }
	 }
}

\author{ Wayne (Yanwei) Zhang \email{actuaryzhang@uchicago.edu} }

\seealso{See also \code{\link{Mse}}. }

\examples{
showClass("MultiChainLadderFit")
}
\keyword{classes}
