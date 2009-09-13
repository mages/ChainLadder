\name{triangle S3 Methods}
\Rdversion{1.1}
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
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{plot}{triangle}(x, t = "b", xlab = "dev. period", ylab = NULL, ...)
\method{as.data.frame}{triangle}(x, na.rm=FALSE)
\method{as.triangle}{data.frame}(Triangle, origin="origin", dev="dev", value="value")
\method{as.triangle}{matrix}(Triangle)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a matrix of class 'triangle'}
  \item{data}{a \code{data.frame} or a \code{data.frame} with
    additional class attribute 'triangle' columns for origin and development periods and a value column,e.g
    claims}
  \item{Triangle}{Triangle}
  \item{data}{triangle.}
  \item{origin}{name of the origin period, default is \code{"origin"}.}
  \item{dev}{name of the origin period, default is \code{"dev"}.}
  \item{origin}{name of the value, default is \code{"value"}.}
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Markus Gesmann
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
\keyword{ plot }% __ONLY ONE__ keyword per line
