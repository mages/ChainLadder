\name{as.LongTriangle}
\alias{as.LongTriangle}
\title{Convert Triangle from wide to long}
\description{
Given a Triangle in matrix ("wide") format,
convert to data.frame ("long") format.
}
\usage{
as.LongTriangle(Triangle, varnames = names(dimnames(Triangle)), 
                value.name = "value", na.rm = TRUE)
}
\arguments{
    \item{Triangle}{a loss "triangle". Must be a \code{matrix}.}
    \item{varnames}{\code{character} names for the columns that will store the 
    \code{rownames} and \code{colnames} of matrix \code{Triangle}. 
    Defaults to \code{names(dimnames(Triangle))} if available.
    If not provided, uses c("origin", "dev").}
    \item{value.name}{column name to be given to the matrix
    values that will be stored in the data.frame. 
    Defaults to "value".}
    \item{na.rm}{should NA values be excluded from the data.frame?
    Defaults to TRUE.}
    }
\details{
Unlike the as.data.frame.triangle method,
and Unlike the 'melt' method in the 'reshape2' package, 
this function returns a data.frame where the rownames and colnames of Triangle
are stored as \emph{factors}.
This can be a critical feature when the order of the levels of the columns
is important.
For example, when a Triangle is plotted, the order of the
origin and dev dimensions is important.
See Examples section.
}
\value{
A \code{data.frame}.
}
\author{
Daniel Murphy
}
\seealso{
  \code{as.data.frame.triangle}
}
\examples{
as.LongTriangle(GenIns)
\dontrun{
ggplot(as.LongTriangle(GenIns), 
       aes(x = dev, y = value, group = origin, color = origin)) + geom_line()
}
}
