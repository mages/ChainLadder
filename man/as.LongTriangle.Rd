\name{as.LongTriangle}
\alias{as.LongTriangle}
\title{Convert Triangle from wide to long}
\description{
Given a Triangle in matrix ("wide") format,
convert to data.frame ("long") format.
}
\usage{
as.LongTriangle(Triangle, varnames = names(dimnames(Triangle)), ..., 
                na.rm = TRUE, as.is = TRUE, value.name = "value")
}
\arguments{
    \item{Triangle}{a loss "triangle". Must be a \code{matrix}.}
    \item{varnames}{\code{character} names for the columns that will store the 
    \code{rownames} and \code{colnames} of matrix \code{Triangle}. 
    Defaults to \code{names(dimnames(Triangle))} if available.
    If not provided, uses c("origin", "dev").}
    \item{\dots}{arguments passed to \code{reshape2::melt}}
    \item{na.rm}{should NA values be excluded from the data.frame?
    Defaults to TRUE.}
    \item{as.is}{should \code{rownames(Triangle)} and \code{colnames(Triangle)}
    remain "as.is" -- \code{character} -- values in the data.frame or
    should they be converted, if possible,
    to other supported modes? 
    Default is TRUE. 
    For more information, including supported modes, 
    see help for \code{melt} in package \code{reshape2}.}
    \item{value.name}{column name to be given to the matrix
    values that will be stored in the data.frame. 
    Defaults to "value".}
    }
\details{
Based on the 'melt' method in the 'reshape2' package.
}
\value{
A \code{data.frame}.
}
\author{
Daniel Murphy
}
\seealso{
  \code{reshape2::melt}
}
\examples{
as.LongTriangle(GenIns)
}
