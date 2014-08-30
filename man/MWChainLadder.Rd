\name{MWChainLadder}
\alias{MWChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Claims development result
}
\description{
Standard deviation of the claims development result after one year for the distribution-free chain-ladder model (Mack).
}
\usage{
MWChainLadder(Mack)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mack}{Output of \code{\link{MackChainLadder}}.
}
}
\details{
Merz & Wuethrich (2008) derived analytic formulae for the standard deviation of the claims development result after one year assuming:
\itemize{
  \item{
    The opening reserves were set using the pure chain ladder model 
    (no tail)
    }
  \item{
    Claims develop in the year according to the assumptions underlying
    Mack's model
    }
  \item{
    Reserves are set after one year using the pure chain ladder model 
    (no tail) 
    }
  }
}
\value{
A \code{data.frame} with various ultimate and one-year statistics of the 
claims development result.
}
\references{
\cite{Modelling the claims development result for solvency purposes.
Michael Merz, Mario V. Wuethrich. Casualty Actuarial Society E-Forum, Fall 2008.}
}
\author{
Arthur Charpentier, Arnaud Lacoume
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{MackChainLadder}}
}
\examples{
# Example from Merz Wuethrich paper mentioned above
MWR 
M <- MackChainLadder(MWR, est.sigma="Mack")
plot(M)
MWChainLadder(M)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
