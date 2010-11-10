\name{ClarkLDF}
\alias{ClarkLDF}
\title{
Clark LDF method
}
\description{
Analyze loss triangle using Clark's loss development factor (LDF) method.
}
\usage{
ClarkLDF(data, cumulative = TRUE, adol = TRUE, 
        maxage = c(Inf, Inf), G = "loglogistic")
}
\arguments{
  \item{data}{
A loss triangle in the form of a matrix.
The column names of the matrix should be able to be interpreted
as the "age" of the losses in that column.
The row names of the matrix should uniquely define the year of
origin of the losses in that row.
Losses may be inception-to-date or incremental.
}
  \item{cumulative}{
If \code{TRUE} (the default), values in \code{data} are
inception to date.
If \code{FALSE}, \code{data} holds incremental losses.
}
  \item{adol}{
If \code{TRUE} (the default), the growth function should be applied
to the length of time from the average date of loss ("adol")
of losses in the origin year.
If \code{FALSE}, the growth function should be applied
to the length of time since the beginning of the origin year.
}
  \item{maxage}{
The "ultimate" age to which losses should be projected.
This can be a vector of length 1 or 2.
The first element holds the ultimate age as traditionally defined, 
namely the length of time since the beginning of the origin year.
If the second element exists --
which is pertinent only in the case that \code{adol} is \code{TRUE} --
it holds the length of time from the
average date of loss of the origin year.
If the second element does not exist and \code{adol} is \code{TRUE},
its value will be derived as \code{maxage[1]} minus 
the average difference between the ages in \code{colnames(data)},
with a warning if not all differences are the same.
}
  \item{G}{
A \code{character} scalar identifying the "growth function."
The two growth functions defined at this time are "loglogistic"
(the default)
and "weibull".
}
}
\details{
Clark's "Cape Cod" method assumes that the 
incremental losses across development periods in a loss triangle
are independent.
He assumes that the expected value of an incremental loss is
equal to the \emph{theoretical} expected ultimate loss (\bold{U})
(by origin year)
times the change in the \emph{theoretical}
underlying growth function over the development period.
Clark models the growth function, also called the percent of ultimate,
by either the loglogistic function (a.k.a., "the inverse power curve") 
curve or the weibull function.
Clark completes his incremental loss model 
by wrapping the expected values within an 
overdispersed poisson (ODP) process where
the "scale factor" 
sigma^2
is assumed to be a known constant for all 
development periods.
 
The parameters of Clark's "Cape Cod" method are therefore:
U,
and 
omega
and 
theta
(the parameters of the \bold{loglogistic} and \bold{weibull} growth functions).
Finally, Clark uses maximum likelihood to parameterize his model,
uses the ODP process to estimate process risk, and
uses the Cramer-Rao theorem and the "delta method" to estimate parameter risk.

Clark recommends inspecting the residuals to help assess the 
reasonableness of the model relative to the actual data
(see \code{\link{plot.clark}} below).

}
\value{
A \code{list} of class "clark" with the following components:
\itemize{
    \item method: "LDF"
    \item growthFunction: name of the growth function
    \item Table65: the table of losses, standard errors and CVs as shown
    on p. 65 of the paper
    \item Table64: the table of ages, growth function values, LDFs,
    truncated LDFs, "ultimate" losses, and "reserves" as shown on p. 64 
    of the paper
    \item par: the estimated parameters
    \item sigma2: the sigma-squared "scale parameter"
    \item LDF: the vector of to-ultimate loss development factors
    \item origin: rownames(data) from the "long format" of \code{data}
    \item age: colnames(data) from the "long format" of \code{data}
    \item fitted: the expected values (the "mu's") of the incremental losses
    \item residuals: the difference between the actual and fitted values
    \item stdresid: the standardized residuals 
    = residuals/sqrt(sigma2*fitted)
    (referred to as "normalized residuals" in the paper; see p. 62)
}
}
\references{
Clark, David R., 
"LDF Curve-Fitting and Stochastic Reserving: A Maximum Likelihood Approach",
\emph{Casualty Actuarial Society Forum}, Fall, 2003
\url{http://www.casact.org/pubs/forum/03fforum/03ff041.pdf} 
}
\author{
Daniel Murphy
}
\seealso{
\code{\link{ClarkCapeCod}}
}
\examples{
X <- GenIns
ClarkLDF(X, maxage=20)
# Clark's method also works with triangles that have  
# more development periods than origin periods
ClarkLDF(qincurred, G="weibull")
}
\keyword{ models }
