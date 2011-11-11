\name{triangle S3 Methods}
\Rdversion{1.1}
\alias{triangle}
\alias{plot.triangle}
\alias{as.data.frame.triangle}
\alias{as.triangle}
\alias{as.triangle.data.frame}
\alias{as.triangle.matrix}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Generic functions for triangles
}
\description{
 Functions to ease the work with triangle shaped matrix data. A
 'triangle' is a matrix with some generic functions.
 Triangles are usually stored in a 'long' format in data bases. The
 function \code{as.triangle} can transform a \code{data.frame} into a
 triangle shape.
}
\usage{
\method{as.triangle}{matrix}(Triangle,origin="origin", dev="dev", value="value",...)
\method{as.triangle}{data.frame}(Triangle, origin="origin", dev="dev", value="value",...)
\method{as.data.frame}{triangle}(x, row.names=NULL, optional, lob=NULL, na.rm=FALSE, ...)
as.triangle(Triangle, origin="origin", dev="dev", value="value",...)
\method{plot}{triangle}(x, t = "b", xlab = "dev. period", ylab = NULL, lattice=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{a triangle}
  \item{origin}{name of the origin period, default is \code{"origin"}.}
  \item{dev}{name of the development period, default is \code{"dev"}.}
  \item{value}{name of the value, default is \code{"value"}.}
  \item{row.names}{default is set to \code{NULL} an will merge origin
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
  \item{t}{type, see \code{\link{plot.default}}}
  \item{\dots}{arguments to be passed to other methods}
}
%\details{
  %%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Markus Gesmann
}
\section{Warning}{
Please note that for the function \code{as.triangle} the origin and
dev. period columns have to be of type numeric or a character which can be
converted into numeric.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
%\keyword{ plot }% __ONLY ONE__ keyword per line
