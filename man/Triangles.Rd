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
 function \code{as.triangle} can transform \code{data.frames} into a
 triangle shape.
}
\usage{
\method{as.triangle}{matrix}(Triangle,origin="origin", dev="dev", value="value",...)
\method{as.triangle}{data.frame}(Triangle, origin="origin", dev="dev", value="value",...)
\method{as.data.frame}{triangle}(x, row.names, optional, na.rm=FALSE, ...)
as.triangle(Triangle, origin="origin", dev="dev", value="value",...)
\method{plot}{triangle}(x, t = "b", xlab = "dev. period", ylab = NULL, lattice=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangle}{a triangle}
  \item{origin}{name of the origin period, default is \code{"origin"}.}
  \item{dev}{name of the origin period, default is \code{"dev"}.}
  \item{value}{name of the value, default is \code{"value"}.}
  \item{row.names}{not used}
  \item{optional}{not used}
  \item{na.rm}{logical. Remove missing values?}
  \item{x}{a matrix of class 'triangle'}
  \item{data}{a \code{data.frame} or a \code{data.frame} with
    additional class attribute 'triangle' columns for origin and development periods and a value column,e.g
    claims}
    \item{xlab}{a label for the x axis, defaults to 'dev. period'}
  \item{ylab}{a label for the y axis, defaults to NULL}
  \item{lattice}{logical. If \code{FALSE} the function
    \code{\link{matplot}} is used to plot the developments of the
    triangle in one graph, otherwise the \code{\link{xyplot}} function
    of the lattice package is used, to plot developments of each origin
    period in a different panel.}
  \item{t}{type, see \code{\link{plot.default}}}
  \item{\dots}{Arguments to be passed to methods}
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
%\note{
%%  ~~further notes~~
%}

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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
%\keyword{ plot }% __ONLY ONE__ keyword per line
