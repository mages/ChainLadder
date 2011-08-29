\name{JoinFitMse}
\alias{JoinFitMse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Join Model Fit and Mse Estimation }
\description{
  This function combines first momoent estimation from fitted regression models and second moment estimation from \code{Mse} method to construct an object of class "MultiChainLadder", for which a variety of methods are defined, such as \code{summary} and \code{plot}. 
}
\usage{
JoinFitMse(models, mse.models)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{models}{ fitted regression models, either of class "MCLFit" or "GMCLFit". }
  \item{mse.models}{ output from a call to \code{Mse}, which is of class "MultiChainLadderMse". }
}
	
\author{Wayne Zhang \email{actuary_zhang@hotmail.com}}

\seealso{ See also \code{\link{MultiChainLadder}.}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

