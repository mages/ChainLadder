\name{Cumulative and incremental triangles}
\alias{incr2cum}
\alias{cum2incr}
\title{Cumulative and incremental triangles}
\description{
 Functions to convert between cumulative and incremental triangles
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
}
\details{
  \code{incr2cum} transforms an incremental triangle into a cumulative
  triangle, \code{cum2incr} provides the reserve operation.
}
\value{
 Both functions return a \code{triangle}.  
}
\author{Markus Gesmann, Christophe Dutang}
\seealso{ See also \code{\link{as.triangle}}}
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
incMortgage <- cum2incr(Mortgage)
#add missing values
incMortgage[1,1] <- NA
incMortgage[2,1] <- NA
incMortgage[1,2] <- NA

#with missing values argument
incr2cum(incMortgage, na.rm=TRUE)

#compared to 
incr2cum(Mortgage)

}
\keyword{ array }
