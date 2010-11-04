\name{print.clark}
\alias{print.clark}
\title{Print result of Clark method}
\description{
Function to print the reserves, standard errors, and CVs 
of the clarkLDF and clarkCapeCod methodologies.
}
\usage{
\method{print}{clark}(x, \dots)
}
\arguments{
\item{x}{
object resulting from a run of the clarkLDF or clarkCapeCod functions.
}
\item{\dots}{
further arguments passed to \code{print}.
}
}
\details{
The "default" information to display at the console resulting from 
a run of the "LDF Method" or "Cape Cod Method".
}
\value{
The table as shown
on pages 65 and 69 of the paper (see "References").
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
library(ChainLadder)
X <- GenIns
colnames(X) <- 12*as.numeric(colnames(X))
CC.loglogistic  <- clarkCapeCod(X, Premium=10000000+400000*0:9, maxage=240)
print(CC.loglogistic)
}
