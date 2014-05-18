\name{LRfunction}
\Rdversion{1.1}
\alias{LRfunction}
\title{ Calculate the Link Ratio Function  }
\description{
This calculates the link ratio function per the CLFM paper.
}
\usage{
LRfunction(x, y, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{x}{beginning value of loss during a development period}
   \item{y}{ending value of loss during a development period}
  \item{delta}{numeric}
  }

\details{
 
Calculated the link ratios resulting from a chainladder model
over a development period indexed by (possibly vector valued) real number delta.
See formula (5) in the References.
 }
\value{
  A vector of link ratios.
}
\references{
  
\cite{Bardis, Majidi, Murphy. A Family of Chain-Ladder Factor Models for Selected Link Ratios. \emph{Variance}. Pending. 2013. pp.tbd:tbd}

}
\author{
  Dan Murphy
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

x <- RAA[1:9,1]
y <- RAA[1:9,2]
delta <- seq(-2, 2, by = .1)
plot(delta, LRfunction(x, y, delta), type = "l")
}
\keyword{ models }
