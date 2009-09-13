\name{Utility functions}
\alias{incr2cum}
\alias{cum2incr}
\title{Utility functions}
\description{
Utility functions to handle reserving triangles
}
\usage{
incr2cum(Triangle, na.rm=FALSE)
cum2incr(Triangle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{triangle. Assume columns are the development
	period, use transpose otherwise.}
  \item{na.rm}{logical. Should missing values be removed?}
\item{x}{\code{data.frame} of a triangle in long format with}
}
%\details{
%I don't know  
%
%}
\value{
  \code{incr2cum} transforms an incremental triangle into a cumulative triangle, \code{cum2incr} the reserve operation
  \code{as.data.frame.matrix} returns a \code{data.frame} with three columns.
  \code{as.matrix.triangle} returns a \code{matrix}. 
}
\author{Markus Gesmann \email{markus.gesmann@gmail.com}
Christophe Dutang}
\seealso{ See also \code{\link{qpaid}}, 
  \code{\link{RAA}}, 
  \code{\link{plot.MackChainLadder}}, 
  \code{\link{residuals.MackChainLadder}}, 
  \code{\link{MunichChainLadder}},
  \code{\link{BootChainLadder}},
}
\examples{

# See the Taylor/Ashe example in Mack's 1993 paper

#original triangle
GenIns

#incremental triangle
cum2incr(GenIns)

#original triangle
incr2cum(cum2incr(GenIns))

# See the example in Mack's 1999 paper

#original triangle
Mortgage

#add missing values
Mortgage[1,1] <- NA
Mortgage[2,1] <- NA
Mortgage[1,2] <- NA

#with missing values argument
cum2incr(Mortgage)

#compared to 
cum2incr(Mortgage)

}
\keyword{ array }
