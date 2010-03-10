\name{triangles-class}
\docType{class}
\alias{triangles-class}
\alias{[,triangles,missing,numeric,logical-method}
\alias{[,triangles,missing,numeric,missing-method}
\alias{[,triangles,numeric,missing,logical-method}
\alias{[,triangles,numeric,missing,missing-method}
\alias{[,triangles,numeric,numeric,missing-method}
\alias{[<-,triangles,numeric,numeric,list-method}
\alias{coerce,list,triangles-method}
\alias{dim,triangles-method}
\alias{cbind2,triangles,missing-method}
\alias{rbind2,triangles,missing-method}

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
    \item{[}{\code{signature(x = "triangles", i = "missing", j = "numeric", drop = "logical")}: Method for primitive function "[" to subset certain columns. If \code{drop=TRUE}, rows composed of all "NA"s are removed. Dimensions are not dropped. }
  \item{[}{\code{signature(x = "triangles", i = "missing", j = "numeric", drop = "missing")}: Method for primitive function "[" to subset certain columns, where rows composed of all "NA"s are removed. Dimensions are not dropped. }
  \item{[}{\code{signature(x = "triangles", i = "numeric", j = "missing", drop = "logical")}: Method for primitive function "[" to subset certain rows. If \code{drop=TRUE}, columns composed of all "NA"s are removed. Dimensions are not dropped. }
  \item{[}{\code{signature(x = "triangles",  i = "numeric", j = "missing", drop = "missing")}: Method for primitive function "[" to subset certain rows, where columns composed of all "NA"s are removed. Dimensions are not dropped. }
  \item{[}{\code{signature(x = "triangles", i = "numeric", j = "numeric", drop = "missing")}: Method for primitive function "[" to subset certain rows and columns. Dimensions are not dropped. }
 \item{[<-}{\code{signature(x = "triangles", i = "numeric", j = "numeric", value = "list")}: Method for primitive function "[<-" to replace one cell in each triangle  with values specified in \code{value}.  }
    \item{coerce}{\code{signature(from = "list", to = "triangles")}: Method to construct a "triangles" object from "list". }
    \item{dim}{\code{signature(x = "triangles")}: Method to get the dimensions. The return value is a vector of length 3, where the first element is the number of triangles, the sencond is the number of accident years, and the third is the number of development years. }
  \item{cbind2}{\code{signature(x = "triangles", y="missing")}: Method to column bind all triangles using \code{cbind} internally.  }
  \item{rbind2}{\code{signature(x = "triangles", y="missing")}:  Method to row bind all triangles using \code{rbind} internally.  }
}
}

\author{ Wayne (Yanwei) Zhang \email{actuaryzhang@uchicago.edu}}



\seealso{ See also \code{\link{MultiChainLadder}}
}

\examples{
data(auto)

# "coerce"
auto <- as(auto,"triangles")  # transform "list" to be "triangles"

# method for "["
auto[,4:6,drop=FALSE] # rows of all NA's not dropped
auto[,4:6]			# drop rows of all NA's

auto[8:10, ,drop=FALSE] #columns of all NA's not dropped
auto[8:10, ] 			#columns of all NA's  dropped

auto[1:2,1]

# replacement method
auto[1:2,1] <- list(1,2,3)
auto[1,2]
   
dim(auto)

cbind2(auto[1:2,1])
rbind2(auto[1:2,1])
}
\keyword{classes}
