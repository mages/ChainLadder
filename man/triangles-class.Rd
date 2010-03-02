\name{triangles-class}
\docType{class}
\alias{triangles-class}
\alias{[,triangles,missing,numeric,missing-method}
\alias{coerce,list,triangles-method}

\title{S4 Class "triangles"  }
\description{This is a S4 class that has "list" in the data part. This class is created to facilitate validation and extration of data. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("triangles", ...)}, or use \code{as(...,"triangles")}, where \code{...} is a "list".
}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} }
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{Mse}{\code{signature(ModelFit = "GMCLFit", FullTriangles = "triangles")}: See \code{\link{Mse}} }
    \item{Mse}{\code{signature(ModelFit = "MCLFit", FullTriangles = "triangles")}:  See \code{\link{Mse}} }
    \item{[}{\code{signature(x = "triangles", i = "missing", j = "numeric", drop = "missing")}: Method for primitive function "[" to subset certain columns, where rows composed of all "NA"s are removed. }
    \item{coerce}{\code{signature(from = "list", to = "triangles")}: Method to construct a "triangles" object from "list". }
}
}

\author{ Wayne (Yanwei) Zhang \email{actuaryzhang@uchicago.edu}}



\seealso{ See also \code{\link{MultiChainLadder}}
}

\examples{
data(liab)
liab <- as(liab,"triangles")  # transform "list" to be "triangles"
liab2 <- liab[,1:10]   

}
\keyword{classes}
