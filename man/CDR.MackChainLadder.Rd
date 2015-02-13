\name{CDR.MackChainLadder}
\alias{CDR.MackChainLadder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Claims development result
}

\encoding{UTF-8}

\description{
Standard deviation of the claims development result after one year for the distribution-free chain-ladder model (Mack).
}
\usage{
CDR.MackChainLadder(Mack)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mack}{Output of \code{\link{MackChainLadder}}.
}
}
\details{
Merz & Wüthrich (2008) derived analytic formulae for the mean square error
of prediction of the claims development result after one year assuming:
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
Michael Merz, Mario V. Wüthrich. Casualty Actuarial Society E-Forum, Fall 2008.}

\cite{Claims Run-Off Uncertainty: The Full Picture. Michael Merz, 
Mario V. Wüthrich. Swiss Finance Institute Research Paper No. 14-69. http://ssrn.com/abstract=2524352. 2014}
}
\author{
Arthur Charpentier and Arnaud Lacoume with contributions from Mario Wüthrich
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{MackChainLadder}}
}
\examples{
# Example from the 2008 Merz Wuthrich paper mentioned above
MW2008
M <- MackChainLadder(MW2008, est.sigma="Mack")
plot(M)
CDR.MackChainLadder(M)

# Example from the 2014 Merz Wuthrich paper mentioned above
MW2014
W <- MackChainLadder(MW2014, est.sigma="Mack")
plot(W)
CDR.MackChainLadder(W)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
