\name{ata}
\alias{ata}
\title{Calculate Age-to-Age Factors}
\description{
Calculate the matrix of age-to-age factors 
(also called "report-to-report" factors, or "link ratios")
for an object of class \code{triangle}.
}
\usage{
ata(Triangle, NArow.rm = TRUE, colname.sep = "-",
        colname.order=c("ascending","descending"))

}
\arguments{
    \item{Triangle}{a loss "triangle". Must be a \code{matrix}.}
    \item{NArow.rm}{\code{logical} indicating if 
        rows of age-to-age (ata) factors that are all \code{NA}
        should be removed.
        "All-NA" rows typically occur for the most recent origin year
        of a loss triangle.}
    \item{colname.sep}{a \code{character} indicating the separator
        character to place between the column names of \code{Triangle}
        that will be used to lable the columns of the resulting 
        matrix of ata factors}
    \item{colname.order}{"ascending" indicates that the less mature
        age comes first in the column labels of the ata matrix}
    }
\details{
\code{ata} constructs a matrix of age-to-age (ata) factors resulting
from a loss "triangle" or a \code{matrix}.
Simple averages and volume weighted averages are saved as 
"smpl" and "vwtd" attributes, respectively.

}
\value{
A \code{matrix} with "smpl" and "vwtd" attributes.
}
\author{
Daniel Murphy
}
\seealso{
\code{\link{summary.ata}} and \code{\link{print.ata}}}
\examples{
ata(GenIns)

# Volume weighted average age-to-age factor of the "RAA" data
y <- attr(ata(RAA), "vwtd")
y
# "To ultimate" factors with a 10% tail
y <- rev(cumprod(rev(c(y, 1.1))))
names(y) <- paste(colnames(RAA), "Ult", sep="-")
y

## Label the development columns in "ratio-type" format
ata(RAA, colname.sep=":", colname.order="desc")

}
