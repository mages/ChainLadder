\name{CLFMdelta}
\Rdversion{1.1}
\alias{CLFMdelta}
\title{ Find "selection consistent" values of delta  }
\description{
This function finds the values of delta,
one for each development period, 
such that the model coefficients resulting from 
the 'chainladder' function with delta = solution are consistent
with an input vector of 'selected' development age-to-age factors.
}
\usage{
%CLFMdelta(Triangle, selected, tolerance = .0005, step.a = 1, ...)
CLFMdelta(Triangle, selected, tolerance = .0005, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{Triangle}{cumulative claims triangle.  A (mxn)-matrix \eqn{C_{ik}} 
    which is filled for \eqn{k \leq n+1-i; i=1,\ldots,m; m\geq n }, see
    \code{\link{qpaid}} for how to use (mxn)-development triangles with
    m<n, say higher development period frequency (e.g quarterly) than
    origin period frequency (e.g accident years).}
  \item{selected}{ a vector of selected age-to-age factors or "link ratios",
    one for each development period of 'Triangle'}
  \item{tolerance}{a 'tolerance' parameters. Default: .0005;
    for each element of 'selected' 
    a solution 'delta' will be found -- if possible --
    so that the chainladder model indexed by
    'delta' results in a multiplicative coefficient within 'tolerance' 
    of the 'selected' factor.}
%  \item{step.a}{the starting width of the search intervals}
  \item{\dots}{ not in use}
  }

\details{
 
For a given input Triangle and vector of selected factors, 
a search is conducted for chainladder models that are "consistent with" the 
selected factors. 
By "consistent with" is meant that the coefficients of the \code{\link{chainladder}} 
function are equivalent to the selected factors.
Not all vectors of selected factors can be considered consistent with a given
Triangle; 
feasibility is subject to restrictions on the 'selected' factors relative to
the input 'Triangle'.
See the References.


The default average produced by the \code{chainladder} function is the 
volume weighted average and corresponds to \code{delta = 1} in the call
to that function;
\code{delta = 2} produces the simple average; and
\code{delta = 0} produces the "regression average", i.e., 
the slope of a regression line fit to the data 
and running through the origin.
By convention, if the \code{selected} value corresponds to 
the volume-weighted average, the simple average, or the regression average,
then the value returned will be 1, 2, and 0, respectively. 

Other real-number values for \code{delta} will produce a different average.
The point of this function is to see if there exists a model as determined
by delta whose average is consistent with the value in the 
\code{selected} vector.
That is not always possible. See the References.

It can be the case that one or more of the above three "standard averages" 
will be quite close to each other 
(indistinguishable within \code{tolerance}).
In that case, the value returned will be, in the following priority order
by convention,
1 (volume weighted average),
2 (simple average), or
0 (regression average).

 }
\value{
  A vector of real numbers, say delta0, such that 
  \code{coef(chainladder(Triangle, delta = delta0))} = \code{selected}
  within \code{tolerance}.
  A \code{logical} attribute 'foundSolution' indicates if a solution was
  found for each element of \code{selected}.
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
F <- y/x
CLFMdelta(RAA[1:9, 1:2], selected = mean(F)) # value is 2, 'foundSolution' is TRUE
CLFMdelta(RAA[1:9, 1:2], selected = sum(y) / sum(x)) # value is 1, 'foundSolution' is TRUE

selected <- mean(c(mean(F), sum(y) / sum(x))) # an average of averages
CLFMdelta(RAA[1:9, 1:2], selected) # about 1.725
CLFMdelta(RAA[1:9, 1:2], selected = 2) # negative solutions are possible

# Demonstrating an "unreasonable" selected factor.
CLFMdelta(RAA[1:9, 1:2], selected = 1.9) # NA solution, with warning

}
\keyword{ models }
