\name{extract.cols}
\alias{extract.cols}

\title{ Extract columns from a list }
\description{
	This function extracts specified columns from a list of triangles and puts them into the triangle format.
}
\usage{
extract.cols(da.list,cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{da.list}{a list of cumulative claims triangles.} 
  \item{cols}{ column indexes to be extracted.} 
}

\author{Yanwei (Wayne) Zhang \email{actuaryzhang@uchicago.edu}}

\seealso{ See also \link{MultiChainLadder}, \link{GeneralMultiChainLadder}
}

\examples{

data(auto)
da=auto
da1=extract.cols(da,1:7)	# first 7 columns
da2=extract.cols(da,7:10)	# last 4 columns

da1
da2

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
