\name{plot.clark}
\alias{plot.clark}
\title{Plot Clark method residuals}
\description{
Function to plot the residuals of the Clark LDF and Cape Cod methods.
}
\usage{
\method{plot}{clark}(x, \dots)
}
\arguments{
\item{x}{
object resulting from a run of the clarkLDF or clarkCapeCod functions.
}
\item{\dots}{
not used.
}
}
\details{
This function creates four plots of standardized residuals on a single page:
\enumerate{
    \item By origin
    \item By age
    \item By fitted value
    \item Normal Q-Q plot with results of Shapiro-Wilk test
}
On the first three plots are also shown a linear fit in blue and 
a lowess smoothed fit in red.
}
\references{
Clark, David R., 
"LDF Curve-Fitting and Stochastic Reserving: A Maximum Likelihood Approach",
\emph{Casualty Actuarial Society Forum}, Fall, 2003
}
\author{
Daniel Murphy
}
\seealso{
clarkLDF, clarkCapeCod
}
\examples{
require(ChainLadder)
plot(clarkLDF(GenIns, maxage=20, G="weibull"))
}
