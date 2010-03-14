\name{ChainLadder Database Examples}

\alias{getDatabaseExampleData}
\alias{DataTableToTriangles}
\alias{ChainLadderSummaries}
\alias{writeDatabaseExampleResults}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
These function are designed to give the user a starting point to develop their own database interfaces.
}
\description{
The functions described here should really be regarded only as a starting point for any further development by the user.
These function give illustrative examples of how the ChainLadder package can be embedded in  
}
\usage{
getDatabaseExampleData()
DataTableToTriangles(data, origin = "origin", dev = "dev", value = "value", lob = "lob", inputCum = FALSE, outputCum = TRUE)
ChainLadderSummaries(CL, summary.by = "ByOrigin")
writeDatabaseExampleResults(data, tablename = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}

  \item{origin}{
%%     ~~Describe \code{origin} here~~
}
  \item{dev}{
%%     ~~Describe \code{dev} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{lob}{
%%     ~~Describe \code{lob} here~~
}
  \item{inputCum}{
%%     ~~Describe \code{inputCum} here~~
}
  \item{outputCum}{
%%     ~~Describe \code{outputCum} here~~
}
  \item{CL}{
%%     ~~Describe \code{CL} here~~
}
  \item{summary.by}{
%%     ~~Describe \code{summary.by} here~~
}
   \item{tablename}{
%%     ~~Describe \code{tablename} here~~
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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
\keyword{ models }