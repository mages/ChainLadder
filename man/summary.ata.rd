\name{summary.ata}
\alias{summary.ata}
\title{ Summary method for object of class 'ata' }

\description{
  Summarize the age-to-age factors resulting from a 
  call to the \code{\link{ata}} function.
}
\usage{
\method{summary}{ata}(object, digits=3, \dots)

}
\arguments{
    \item{object}{object resulting from a call to \code{\link{ata}}}

    \item{digits}{integer indicating the number of decimal places
        for rounding the factors.
        The default is 3.
        \code{NULL} indicates that rounding should take place.
        }
    \item{\dots}{not used}

}
\details{
A call to \code{\link{ata}} produces a matrix of age-to-age factors
with two attributes -- 
the simple and volume weighted averages.
\code{summary.ata} creates a new matrix with the averages appended
as rows at the bottom.

}
\value{
A \code{matrix}.
}

\author{ Dan Murphy }
\seealso{ See also \code{\link{ata}} and \code{\link{print.ata}} }
\examples{
y <- ata(RAA)
summary(y, digits=4)

}
\keyword{ summary }
