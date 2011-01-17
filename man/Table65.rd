\name{Table65}
\alias{Table65}
\alias{Table64}
\alias{Table68}
\title{
    Functions to Reproduce Clark's Tables
}
\description{
    Print the tables on pages 64, 65, and 68 of Clark's paper.
}
\usage{
Table64(x)
Table65(x)
Table68(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object resulting from \code{ClarkLDF} or \code{ClarkCapeCod}}
}
\details{
These exhibits give some of the details behind the calculations producing
the estimates of future values (a.k.a. "Reserves" in Clark's paper).
Table65 works for both the "LDF" and the "CapeCod" methods.
Table64 is specific to "LDF", Table68 to "CapeCod".
}
\value{
A \code{data.frame}. 
}
\references{
Clark, David R., 
"LDF Curve-Fitting and Stochastic Reserving: A Maximum Likelihood Approach",
\emph{Casualty Actuarial Society Forum}, Fall, 2003
\url{http://www.casact.org/pubs/forum/03fforum/03ff041.pdf} 
}
\author{
Daniel Murphy
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
Table65(ClarkLDF(GenIns, maxage=20))
Table64(ClarkLDF(GenIns, maxage=20))

X <- GenIns
colnames(X) <- 12*as.numeric(colnames(X))
Table65(ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=Inf))
Table68(ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=Inf))

}
\keyword{ methods }
