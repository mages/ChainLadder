\name{getLatestCumulative}
\alias{getLatestCumulative}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Triangle information for most recent calendar period.
}
\description{
  Return most recent values for all origin periods of a cumulative development triangle.
}
\usage{
getLatestCumulative(Triangle, na.values = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{a Triangle in matrix format.}
  \item{na.values}{a vector specifying values that should be 
  considered synonymous with NA when searching for the rightmost non-NA.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A vector of most recent non-'NA' (and synonyms, if appropriate) values
  of a triangle for all origin periods.
  The \code{names} of the vector equal the origin names of the Triangle.
  The vector will have additional attributes:
  "latestcol" equalling the index of the column in Triangle corresponding
  to the row's rightmost entry;
  "rowsname" equalling the name of the row dimension of Triangle, if any;
  "colnames" equalling the corresponding column name of Triangle, if any;
  "colsname" equalling the name of the column dimension of Triangle, if any.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Ben Escoto, Markus Gesmann, Dan Murphy  
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{as.triangle}}.
}
\examples{
RAA
getLatestCumulative(RAA)
Y <- matrix(c(1,  2,  3,
              4,  5,  0, 
              6, NA, NA), byrow=TRUE, nrow=3)
getLatestCumulative(Y) # c(3, 0, 6) 
getLatestCumulative(Y, na.values = 0) # c(3, 5, 6) 
}
\keyword{ methods }
