\name{print.clark}
\alias{print.clark}
\alias{print.ClarkLDF}
\alias{print.ClarkCapeCod}
\title{Print results of Clark methods}
\description{
Functions to print the results
of the ClarkLDF and ClarkCapeCod methods.
}
\usage{
%- \method{print}{clark}(x, \dots)
\method{print}{ClarkLDF}(x, Amountdigits=0, LDFdigits=3, CVdigits=3, \dots)

\method{print}{ClarkCapeCod}(x, Amountdigits=0, ELRdigits=3, Gdigits=3, CVdigits=3, \dots)

}
\arguments{
    \item{x}{
        object resulting from a run of the ClarkLDF or ClarkCapeCod function.
        }
    \item{Amountdigits}{number of digits to display to the right of the decimal point for "amount" columns}
    \item{LDFdigits}{number of digits to display to the right of the decimal point for the
        loss development factor (LDF) column}
    \item{CVdigits}{number of digits to display to the right of the decimal point for the 
        coefficient of variation (CV) column}
    \item{ELRdigits}{number of digits to display to the right of the decimal point for the
        expected loss ratio (ELR) column}
    \item{Gdigits}{number of digits to display to the right of the decimal point for the
        "growth function factor" column}
\item{\dots}{
further arguments passed to \code{print}
}
}
\details{
Display the default information in "pretty format" resulting from 
a run of the "LDF Method" or "Cape Cod Method" --
a "Development-type" exhibit for Clark's "LDF Method," 
a "Bornhuetter-Ferguson-type" exhibit for Clark's "Cape Cod Method."

As usual, typing the name of such an object at the console
invokes its \code{print} method.
}
\value{
\code{data.frame}s whose columns are the \code{character} representation
of their respective \code{\link{summary.ClarkLDF}} 
or \code{\link{summary.ClarkCapeCod}} \code{data.frame}s.
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
\code{\link{summary.ClarkLDF}} and \code{\link{summary.ClarkCapeCod}}}
\examples{
X <- GenIns
colnames(X) <- 12*as.numeric(colnames(X))
y <- ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=240)
summary(y) 
print(y)  # (or simply 'y') Same as summary(y) but with "pretty formats"

## Greater growth factors when projecting to infinite maximum age
ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=Inf)

}
\keyword{ methods }
\keyword{ print }
