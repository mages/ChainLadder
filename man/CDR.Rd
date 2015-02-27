\name{CDR}
\alias{CDR}
\alias{CDR.MackChainLadder}
\alias{CDR.BootChainLadder}
\alias{CDR.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
One year claims development result
}

\encoding{latin1}

\description{
Standard deviation of the claims development result after one year 
for the distribution-free chain-ladder model (Mack) and Bootstrap model.
}

\usage{
CDR(x, ...)
\method{CDR}{MackChainLadder}(x, year=1, ...)
\method{CDR}{BootChainLadder}(x, probs=c(0.75, 0.95), ...)
\method{CDR}{default}(x, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{otput of either \code{\link{MackChainLadder}} or \code{\link{BootChainLadder}}}
  \item{year}{vector of development periods or \code{"all"}. 
  Currently only applicable for \code{MackChainLadder} output. 
  Defines the years for which the run off claims development result should be returned.
  }
  \item{probs}{only applicable for \code{BootChainLadder} output.
  Define quantiles to be returned.}
  \item{...}{other arguments}
}
\details{
Merz & Wüthrich (2008) derived analytic formulae for the mean square error
of prediction of the claims development result for the 
Mack chain-ladder model after one year assuming:
\itemize{
  \item{
    The opening reserves were set using the pure chain-ladder model 
    (no tail)
    }
  \item{
    Claims develop in the year according to the assumptions underlying
    Mack's model
    }
  \item{
    Reserves are set after one year using the pure chain-ladder model 
    (no tail) 
    }
  }
}
\value{
A \code{data.frame} with various IBNR/reserves and one-year statistics of the 
claims development result.
}
\references{
\cite{Michael Merz, Mario V. Wüthrich. \emph{Modelling the claims development result for solvency purposes.} Casualty Actuarial Society E-Forum, Fall 2008.}

\cite{Michael Merz, Mario V. Wüthrich. \emph{Claims Run-Off Uncertainty: The Full Picture.}  
 Swiss Finance Institute Research Paper No. 14-69. \url{http://ssrn.com/abstract=2524352}. 2014}
}
\author{Mario Wüthrich and Markus Gesmann 
with contributions from Arthur Charpentier and Arnaud Lacoume 
for \code{CDR.MackChainLadder} and Giuseppe Crupi and 
Markus Gesmann for \code{CDR.BootChainLadder}.
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{MackChainLadder}} and \code{\link{BootChainLadder}}
}
\examples{
# Example from the 2008 Merz, Wuthrich paper mentioned above
MW2008
M <- MackChainLadder(MW2008, est.sigma="Mack")
plot(M)
CDR(M)
# Return all run-off result developments
CDR(M, year="all")

# Example from the 2014 Merz, Wuthrich paper mentioned above
MW2014
W <- MackChainLadder(MW2014, est.sigma="Mack")
plot(W)
CDR(W)

# Example with the BootChainLadder function, assuming overdispered Poisson model
B <- BootChainLadder(MW2008, process.distr=c("od.pois"))
B
CDR(B)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
