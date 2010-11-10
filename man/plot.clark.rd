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
If Clark's model is appropriate for the actual data,
then the standardized residuals should appear as
independent standard normal random variables.
This function creates four plots of standardized residuals on a single page:
\enumerate{
    \item By origin
    \item By age
    \item By fitted value
    \item Normal Q-Q plot with results of Shapiro-Wilk test
}
If the model is appropriate then there should not appear to be any trend in the
standardized residuals or any systematic differences in the spread 
about the line y = 0. 
The Shapiro-Wilk p-value shown in the fourth plot gives an indication 
of how closely the standardized residuals can be considered "draws"
from a standard normal random variable.

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
X <- GenIns
Y <- ClarkLDF(GenIns, maxage=Inf, G="weibull")
plot(Y)  # One obvious outlier, shapiro test flunked
X[4,4] <- NA  # remove the outlier
Z <- ClarkLDF(GenIns, maxage=Inf, G="weibull")
plot(Z)  # Q-Q plot looks good
}
