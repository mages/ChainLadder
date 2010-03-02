\name{Join2Fits}
\alias{Join2Fits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Join Two Fitted MultiChainLadder Models }
\description{
  This function is created to facilitate the fitting of the multivariate functions when specifying different models in two different development periods, especially when separate chain ladder is used in later periods.
}
\usage{
Join2Fits(object1, object2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object1}{ An object of class "MultiChainLadder" }
  \item{object2}{ An object of class "MultiChainLadder" }
}
\details{
  The inputs must be of class  "MultiChainLadder" because this function depends on the \code{model} slot to determine what kind of object is to be created and returned. If both objects have \code{"MCL"}, then an object of class "MCLFit" is created; if one has \code{"GMCL"} and one has \code{"MCL"}, then an object of class "GMCLFit" is created, where the one with \code{"GMCL"} is assumed to come from the first development periods; if both have \code{"GMCL"}, then an object of class "GMCLFit" is created. 

}
\author{Wayne (Yanwei) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See also \code{\link{MultiChainLadder}}
}


\keyword{ models }

