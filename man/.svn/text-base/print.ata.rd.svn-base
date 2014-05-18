\name{print.ata}
\alias{print.ata}
\title{Print Age-to-Age factors}
\description{
Function to print the results
of a call to the \code{ata} function.
}
\usage{
\method{print}{ata}(x, \dots)

}
\arguments{
    \item{x}{object resulting from a call to the \code{\link{ata}} function}
    \item{\dots}{further arguments passed to \code{print}}
    }
\details{
\code{print.ata} simply \code{print}s \code{\link{summary.ata}}.
}
\value{
A \code{summary.ata} matrix, invisibly.
}
\author{
Daniel Murphy
}
\seealso{
\code{\link{ata}} and \code{\link{summary.ata}}}
\examples{
x <- ata(GenIns)

## Print ata factors rounded to 3 decimal places, the summary.ata default
print(x) 

## Round to 4 decimal places and print cells corresponding 
## to future observations as blanks.
print(summary(x, digits=4), na.print="") 

}
\keyword{ print }
