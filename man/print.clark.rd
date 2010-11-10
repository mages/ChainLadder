\name{print.clark}
\alias{print.clark}
\title{Print result of Clark method}
\description{
Function to print the reserves, standard errors, and CVs 
of the ClarkLDF and ClarkCapeCod methodologies.
}
\usage{
\method{print}{clark}(x, \dots)
}
\arguments{
\item{x}{
object resulting from a run of the ClarkLDF or ClarkCapeCod functions.
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
\code{\link{ClarkLDF}}, \code{\link{ClarkCapeCod}}
}
\examples{
X <- GenIns
colnames(X) <- 12*as.numeric(colnames(X))
CC.loglogistic  <- ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=240)
print(CC.loglogistic)
}
\keyword{ methods }
\keyword{ print }
