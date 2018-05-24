\name{triangle S3 Methods}
\Rdversion{1.1}
\alias{triangle}
\alias{plot.triangle}
\alias{as.data.frame.triangle}
\alias{as.triangle}
\alias{as.triangle.data.frame}
\alias{as.triangle.matrix}

\title{
  Generic functions for triangles
}
\description{
  Functions to ease the work with triangle shaped matrix data. A
  'triangle' is a matrix with some generic functions.

  \code{triangle} creates a triangle from the given set of vectors of
  observed data.

  \code{as.triangle} attempts to turn its argument into a triangle.
  Triangles are usually stored in a \dQuote{long} format in data bases. The
  function can transform a \code{data.frame} into a triangle shape.

  \code{as.data.frame} turns a triangle into a data frame.
}
\usage{
triangle(\dots, bycol=FALSE, origin="origin", dev="dev", value="value")

\method{as.triangle}{matrix}(Triangle, origin="origin", dev="dev", value="value", \dots)
\method{as.triangle}{data.frame}(Triangle, origin="origin", dev="dev", value="value", \dots)
\method{as.data.frame}{triangle}(x, row.names=NULL, optional, lob=NULL, na.rm=FALSE, \dots)
as.triangle(Triangle, origin="origin", dev="dev", value="value", \dots)
\method{plot}{triangle}(x, type = "b", xlab = "dev. period", ylab = NULL, lattice=FALSE, \dots)
}
\arguments{
  \item{Triangle}{a triangle}
  \item{bycol}{logical. If \code{FALSE} (the default) the triangle is filled by
    rows, otherwise the triangle is filled by columns.}
  \item{origin}{name of the origin period, default is \code{"origin"}.}
  \item{dev}{name of the development period, default is \code{"dev"}.}
  \item{value}{name of the value, default is \code{"value"}.}
  \item{row.names}{default is set to \code{NULL} and will merge origin
    and dev. period to create row names.}
  \item{lob}{default is \code{NULL}. The idea is to use \code{lob} (line
    of business) as an additional column to label a triangle in a long format, see the
    examples for more details.}
  \item{optional}{not used}
  \item{na.rm}{logical. Remove missing values?}
  \item{x}{a matrix of class 'triangle'}
  %  \item{data}{a \code{data.frame} or a \code{data.frame} with
  %    additional class attribute 'triangle' columns for origin and
  %    development periods and a value column,e.g claims}
  \item{xlab}{a label for the x axis, defaults to 'dev. period'}
  \item{ylab}{a label for the y axis, defaults to NULL}
  \item{lattice}{logical. If \code{FALSE} the function
    \code{\link{matplot}} is used to plot the developments of the
    triangle in one graph, otherwise the \code{\link{xyplot}} function
    of the lattice package is used, to plot developments of each origin
    period in a different panel.}
  \item{type}{type, see \code{\link{plot.default}}}
  \item{\dots}{vectors of data in \code{triangle}; arguments to be
    passed to other methods everywhere else.}
}
\details{
  Function \code{triangle} builds a triangle matrix from the vectors of
  \emph{known} data provided in \code{\dots}. Normally, each of these
  vectors should be one shorter than the preceeding one. The length of
  the first vector dictates the number of development periods or origin
  periods (respectively when \code{bycol} is \code{FALSE} or
  \code{TRUE}).

  The names of the arguments in \code{\dots} for function
  \code{triangle} are retained for row/column names. Similarly, the
  names of the elements of the \emph{first} argument are used as
  column/row names.
}
\author{
  Markus Gesmann, Dan Murphy, Vincent Goulet
}
\section{Warning}{
Please note that for the function \code{as.triangle} the origin and
dev. period columns have to be of type numeric or a character which can be
converted into numeric.

Also note that when converting from a data.frame to a matrix with 
\code{as.triangle}, multiple records with the same \code{origin} and 
\code{dev} will be aggregated.
}
\examples{
GenIns
plot(GenIns)
plot(GenIns, lattice=TRUE)


## Convert long format into triangle
## Triangles are usually stored as 'long' tables in data bases
head(GenInsLong)
as.triangle(GenInsLong, origin="accyear", dev="devyear", "incurred claims")

X <- as.data.frame(RAA)
head(X)

Y <- as.data.frame(RAA, lob="General Liability")
head(Y)

## Basic creation of a triangle from loss development data
triangle(c(100, 150, 175, 180, 200),
         c(110, 168, 192, 205),
         c(115, 169, 202),
         c(125, 185),
         150)

## Same, with named origin periods
triangle("2012" = c(100, 150, 175, 180, 200),
         "2013" = c(110, 168, 192, 205),
         "2014" = c(115, 169, 202),
         "2015" = c(125, 185),
         "2016" = 150)

## Again, with also named development periods
triangle("2012" = c("12 months" = 100,
                    "24 months" = 150,
                    "36 months" = 175,
                    "48 months" = 180,
                    "60 months" = 200),
         "2013" = c(110, 168, 192, 205),
         "2014" = c(115, 169, 202),
         "2015" = c(125, 185),
         "2016" = 150)
}
\keyword{ methods }
